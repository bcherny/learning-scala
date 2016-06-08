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

//def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
//  case f::ff => {
//    val (a, rng2) = f(rng)
//    (a::ff.map(_(rng)), rng2)
//  }
//}

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
  val (a, rng2) = fs.head(rng)
  (a::fs.tail.map(_(rng)), rng2)
}

val a: Rand[Int] = rng => (10, rng)
val b: Rand[Int] = rng => (20, rng)
val c: Rand[Int] = rng => (30, rng)
val rng = SimpleRNG(42)
val (res, rng2) = sequence(List(a, b, c))(rng)

assert(res, 20)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}