def tail (l: List[Any]) = l match {
  case Nil => Nil
  case _ :: t => t
}

val list = List(1,2,3,4)

assert(tail(list), List(2,3, 4))
assert(tail(Nil), Nil)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}
