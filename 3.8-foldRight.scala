def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B = as match {
  case Nil => z
  case x::xs => f(x, foldRight(xs, z)(f))
}

def sum (ns: List[Int]) =
  foldRight(ns, 0)(_ + _)




assert(sum(List(1,2,3,4)), 10)


assert(
  foldRight(
    List(1,2,3),
    Nil:List[Int]
  )(_::_),
  List(1,2,3)
)



def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}