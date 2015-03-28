

def concat [A](as: List[List[A]]): List[A] = {
  as.foldLeft(Nil:List[A])((a,b) =>
    b.foldLeft(a)((c,d) => c :+ d)
  )
}

assert(
  concat(List(List(), List("foo"), List("bar", "baz"))),
  List("foo", "bar", "baz")
)

assert(
  concat(List(List(1), List(2,3), List(4,5,6))),
  List(1,2,3,4,5,6)
)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}