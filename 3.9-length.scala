assert(
  List(1,2,3,4).foldRight(0)((_,b) => b+1),
  4
)

assert(
  List(5,6,7).foldRight(0)((_,b) => b+1),
  3
)


def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}