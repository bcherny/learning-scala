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

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, r) = rng.nextInt
  (if (n < 0) -(n + 1) else n, r)
}

val rng = SimpleRNG(42)
val (n0, rng0) = nonNegativeInt(rng)
val (n1, rng1) = nonNegativeInt(rng0)
val (n2, rng2) = nonNegativeInt(rng1)
assert(n0 > 0, true)
assert(n1 > 0, true)
assert(n2 > 0, true)
assert(n0 != n1, true)
assert(n1 != n2, true)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}