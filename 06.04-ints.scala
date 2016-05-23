// stream

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

// rng

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

def int(rng: RNG): (Int, RNG) = {
  val (n, r) = rng.nextInt
  (if (n < 0) -(n + 1) else n, r)
}

// fn

def intsStream(rng: RNG): Stream[(Int, RNG)] = {
  val (n, r) = int(rng)
  Stream.cons((n, r), intsStream(r))
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  val tuples = intsStream(rng).take(count).toList
  val ns = tuples.map(_._1)
  val r = tuples.last._2
  (ns, r)
}

// tests

val rng = SimpleRNG(42)
val (ns1, rng1) = ints(5)(rng)
val (ns2, rng2) = ints(5)(rng1)

ns1 foreach(a => assert(a > 0, true))
ns2 foreach(a => assert(a > 0, true))
ns1 zip ns2 foreach(_ match {
  case (a, b) => assert(a == b, false) // silly scala
})
assert(rng == rng1, false)
assert(rng1 == rng2, false)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}