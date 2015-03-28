
def add1 (as: List[Int]): List[Int] = {

  as.foldLeft(Nil:List[Int])((a,b) => a :+ b+1)

}

assert(add1(List()), List())
assert(add1(List(1,2,3)), List(2,3,4))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}