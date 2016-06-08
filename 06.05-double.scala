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

type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def int(rng: RNG): (Int, RNG) = {
  val (n, r) = rng.nextInt
  (if (n < 0) -(n + 1) else n, r)
}

def double: Rand[Double] =
  map(int)(_.toDouble/Int.MaxValue)

val rng = SimpleRNG(42)
val (n0, rng0) = double(rng)
val (n1, rng1) = double(rng0)
val (n2, rng2) = double(rng1)

assert(n0 > 0, true)
assert(n1 > 0, true)
assert(n2 > 0, true)
assert(n0 < 1, true)
assert(n1 < 1, true)
assert(n2 < 1, true)
assert(n0 != n1, true)
assert(n1 != n2, true)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}