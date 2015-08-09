def sequence[A](a: List[Option[A]]): Option[List[A]] =
	Option(a flatMap(aa => aa))

assert(sequence(List(Option(1), Option(2), Option(3))), Option(List(1,2,3)))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}