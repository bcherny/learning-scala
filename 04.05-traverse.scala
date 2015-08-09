// partial solution - 2nd test fails!
// TODO: full solution

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
	Option(a flatMap(b => f(b)))

assert(
	traverse(List(1,2,3))(a => Some(a*2)),
	Some(List(2,4,6))
)
assert(
	traverse(List(1,2,3))(a => None),
	None
)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}