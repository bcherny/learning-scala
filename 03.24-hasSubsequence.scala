

def hasSubsequence [A] (sup: List[A], sub: List[A]): Any = {

  sup
    .zipWithIndex
    .filter(_._1 == sub.head)
    .map(_._2)

}


assert(hasSubsequence(List(1,2,3,4), List(1,2)), true)
assert(hasSubsequence(List(1,2,3,4), List(3,4)), true)
assert(hasSubsequence(List(1,2,3,4), List(1,2,3,4)), true)
assert(hasSubsequence(List(1,2,3,4), List(2,3,4)), true)
assert(hasSubsequence(List(1,2,3,4), List(3,2)), false)
assert(hasSubsequence(List(1,2,3,4), List(5,4,3,2,1)), false)
assert(hasSubsequence(List(1,2,3,4), List(1,2,4)), false)
assert(hasSubsequence(List(1,2,3,4), List(6)), false)


def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}