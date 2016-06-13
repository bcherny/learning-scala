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

// map

def map[A, B](f: Rand[A])(g: A => B): Rand[B] =
  rng => {
    val gg: A => Rand[B] = a => rng => (g(a), rng)
    flatMap(f)(gg)(rng)
  }

val (a, _) = map(f)(a => "foo")(rng)
assert(a, "foo")

// map2
// TODO

//def map2[A, B, C](f: Rand[A], g: Rand[B])(h: (A, B) => C): Rand[C] =
//val (b, _) = map2(f, f)(_ + _)(rng)
//assert(b, 20)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}