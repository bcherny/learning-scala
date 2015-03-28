

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B) : B = as match {
  case Nil => z
  case x::xs => xs.foldRight(f(z,x))((b,a) => f(a,b))
}

def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) : B = as match {
  case Nil => z
  case x::xs => f(x, xs.foldLeft(z)((a,b) => f(b,a)))
}


assert(foldLeft(List(1,2,3,4),0)(_+_), 10)
assert(foldRight(List(1,2,3,4),0)(_+_), 10)


def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}