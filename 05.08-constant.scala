sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h, t) => h()::t().toList
    case Empty => Nil
  }
  def take(n: Int): Stream[A] = n match {
    case 0 => Empty
    case a => this match {
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case Empty => Empty
    }
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

def constant[A](a: A): Stream[A] =
  Stream.cons(a, constant(a))

assert(constant(1).take(5).toList, List(1, 1, 1, 1, 1))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}