
def zipWith [A,B](as: List[A], bs: List[A])(f: (A,A)=>B): List[B] = {

  def loop [A,B](as: List[A], bs: List[A], acc: List[B]): List[B] = (as,bs) match {
    case (Nil, _) => acc
    case (_, Nil) => acc
    case (a::as, b::bs) => loop(as, bs, acc :+ f(a, b))
  }

  loop(as, bs, Nil:List[B])

}

assert(zipWith(List(), List())(_+_), List())
assert(zipWith(List(1,2,3), List(4,5,6))(_+_), List(5,7,9))
assert(zipWith(List("a", "b", "c"), List("d", "e", "f"))(_+_), List("ad", "be", "cf"))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}