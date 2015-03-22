def drop[A](l: List[A], n: Int): List[A] = n match {

  case 0 => l
  case a => drop(l.tail, n-1)

}

assert(
  drop(List(1,2,3,4), 0),
  List(1,2,3,4)
)

assert(
  drop(List(1,2,3,4), 2),
  List(3,4)
)

assert(
  drop(List(1,2,3,4), 4),
  List()
)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}
