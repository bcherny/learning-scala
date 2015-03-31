
def zipWith [A,B](as: List[A], bs: List[A])(f: (A,A)=>B): List[B] = (as,bs) match {
  case (a::at, b::bt) => f(a, b) :: zipWith(at, bt)(f)
  case (_) => Nil
}

assert(zipWith(List(1,2,3), List(4,5,6))(_+_), List(5,7,9))
assert(zipWith(List("a", "b", "c"), List("d", "e", "f"))(_+_), List("ad", "be", "cf"))
assert(zipWith(List("a", "b", "c", "d"), List("d", "e", "f"))(_+_), List("ad", "be", "cf"))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}