
def zipWith [A,B](as: List[A], bs: List[A])(f: (A,A)=>B): List[B] = {

  def loop (as: List[A], bs: List[A], acc: List[B]): List[B] = (as,bs) match {
    case (a::as, b::bs) => loop(as, bs, acc :+ f(a, b))
    case (_) => acc
  }

  loop(as, bs, Nil:List[B])

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