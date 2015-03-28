
def filter [A](as: List[A])(f: A=>Boolean): List[A] = {
  as.foldLeft(Nil:List[A])((a,b) => f(b) match {
    case true => a :+ b
    case false => a
  })
}

assert(filter(List())(Nil), List())
assert(filter(List(1,2,3,4))(_ % 2 == 0), List(2,4))
assert(filter(List(1,2,3,4))(_ > 3), List(4))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}