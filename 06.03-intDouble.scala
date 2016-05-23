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

def double(rng: RNG): (Double, RNG) = {
  val (n, r) = int(rng)
  ((n.toDouble/Int.MaxValue), r)
}

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (i, r) = int(rng)
  val (d, _) = double(rng)
  ((i, d), r)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val (i, r) = int(rng)
  val (d, _) = double(rng)
  ((d, i), r)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (a, r0) = double(rng)
  val (b, r1) = double(r0)
  val (c, r2) = double(r1)
  ((a, b, c), r2)
}

val rng = SimpleRNG(42)
val ((i0, d0), rng0) = intDouble(rng)
val ((i1, d1), rng1) = intDouble(rng0)
val ((i2, d2), rng2) = intDouble(rng1)
val ((d3, i3), _) = doubleInt(rng)
val ((d4, i4), _) = doubleInt(rng0)
val ((d5, i5), _) = doubleInt(rng1)
val ((d6, d7, d8), _) = double3(rng)

List(
  i0, i1, i2, i3, i4, i5,
  d0, d1, d2, d3, d4, d5, d6, d7, d8
) foreach(a =>
  assert(a > 0, true)
)
assert(d6 == d7, false)
assert(d7 == d8, false)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}