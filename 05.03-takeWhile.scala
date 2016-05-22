sealed trait Stream[+A] {
  def take(n: Int): Stream[A] = n match {
    case 0 => Empty
    case a => this match {
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case Empty => Empty
    }
  }
  def drop(n: Int): Stream[A] = n match {
    case 0 => this
    case a => this match {
      case Cons(_, t) => t().drop(n - 1)
      case Empty => Empty
    }
  }
  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) => p(h()) match {
        case true => Cons(h, () => t().takeWhile(p))
        case false => Empty
      }
      case Empty => Empty
    }
  }
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

assert(Stream.apply(1, 2, 3, 4).takeWhile(_ => false).toList, List())
assert(Stream.apply(1, 2, 3, 4).takeWhile(_ => true).toList, List(1, 2, 3, 4))
assert(Stream.apply(1, 2, 3, 4).takeWhile(_ < 3).toList, List(1, 2))
assert(Stream.apply(1, 2, 3, 4).takeWhile(_ < 10).toList, List(1, 2, 3, 4))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}