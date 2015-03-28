

def append [A](as:List[A], a:A): List[A] = {
  as.foldRight(List(a))((a,b) => a :: b)
}


assert(append(List(), "foo"), List("foo"))
assert(append(List(1,2,3,4), 5), List(1,2,3,4,5))


def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}