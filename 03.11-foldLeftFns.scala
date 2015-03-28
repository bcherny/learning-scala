
def length (ns: List[Int]) = ns.foldLeft(0)((a, _) => a + 1)
def product (ns: List[Int]) = ns.foldLeft(1)(_ * _)
def sum (ns: List[Int]) = ns.foldLeft(0)(_ + _)


assert(length(List()), 0)
assert(length(List(1,2,3,4)), 4)

assert(product(List()), 1)
assert(product(List(1,2,3,4)), 24)
assert(product(List(1,2,-3)), -6)

assert(sum(List()), 0)
assert(sum(List(1,2,3,4)), 10)
assert(sum(List(1,2,3,-4)), 2)


def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}