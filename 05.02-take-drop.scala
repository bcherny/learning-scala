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

// take
assert(Stream.apply(1, 2, 3, 4).take(0).toList, List())
assert(Stream.apply(1, 2, 3, 4).take(1).toList, List(1))
assert(Stream.apply(1, 2, 3, 4).take(4).toList, List(1, 2, 3, 4))
assert(Stream.apply(1, 2, 3, 4).take(10).toList, List(1, 2, 3, 4))

// drop
assert(Stream.apply(1, 2, 3, 4).drop(0).toList, List(1, 2, 3, 4))
assert(Stream.apply(1, 2, 3, 4).drop(1).toList, List(2, 3, 4))
assert(Stream.apply(1, 2, 3, 4).drop(4).toList, List())
assert(Stream.apply(1, 2, 3, 4).drop(10).toList, List())

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}