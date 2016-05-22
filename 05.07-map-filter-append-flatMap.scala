sealed trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](fn: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) =>
      Cons(() => fn(a), () => b)
    )

  def filter(fn: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => fn(a) match {
      case true => Cons(() => a, () => b)
      case false => b
    })

  def toList: List[A] = this match {
    case Cons(h, t) => h()::t().toList
    case Empty => Nil
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

assert(Stream.apply(1, 2, 3, 4).map(_*2).toList, List(2, 4, 6, 8))
assert(Stream.apply(1, 2, 3, 4).filter(_ % 2 == 0).toList, List(2, 4))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}