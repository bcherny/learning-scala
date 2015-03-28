

// from 3.18
def map[A,B](as: List[A])(f: A=>B): List[B] = {
  as.foldLeft(Nil:List[B])((a,b) => a :+ f(b))
}

// from 3.15
def concat [A](as: List[List[A]]): List[A] = {
  as.foldLeft(Nil:List[A])((a,b) =>
    b.foldLeft(a)((c,d) => c :+ d)
  )
}

def flatMap[A,B](as: List[A])(f: A=>List[B]): List[B] = {
  concat(map(as)(f))
}

assert(flatMap(List())(Nil), List())
assert(flatMap(List(1,2,3))(i => List(i,i)), List(1,1,2,2,3,3))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}