def setHead [A] (l: List[A], h: A) = l match {
  case Nil => List(h)
  case _ :: t => h :: t
}

assert(
  setHead(List(1,2,3,4), 3),
  List(3,2,3,4)
)

assert(
  setHead(List(Nil), 3),
  List(3)
)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}
