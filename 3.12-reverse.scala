

def reverse[A](as: List[A]): List[A] = {
  as.foldLeft(Nil:List[A])((a,b) => b::a)
}


assert(reverse(List()), List())
assert(reverse(List(1,2,3,4)), List(4,3,2,1))


def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}