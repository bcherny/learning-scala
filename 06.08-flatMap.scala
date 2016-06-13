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

// flatMap

def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (a, rng2) = f(rng)
    val (b, rng3) = g(a)(rng2)
    (b, rng3)
  }

def f: Rand[Int] = rng => (10, rng)
def g: Int => Rand[String] = (a: Int) => rng => (a.toString, rng)
val rng = SimpleRNG(42)
val (n0, rng0) = flatMap(f)(g)(rng)

assert(n0, "10")

// nonNegativeLessThan

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, r) = rng.nextInt
  (if (n < 0) -(n + 1) else n, r)
}

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) rng => (mod, rng) else nonNegativeLessThan(i)
  })

val rng1 = SimpleRNG(42)
val (a, _) = nonNegativeLessThan(10)(rng1)
assert(a < 10, true)
assert(a > 0, true)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}