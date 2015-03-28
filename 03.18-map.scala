
def map[A,B](as: List[A])(f: A=>B): List[B] = {

  as.foldLeft(Nil:List[B])((a,b) => a :+ f(b))

}

assert(map(List())(Nil), List())
assert(map(List(1,2,3))(_+1), List(2,3,4))
assert(map(List(1,2,3))(_*2), List(2,4,6))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}