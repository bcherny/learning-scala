

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

// from 3.20
def flatMap[A,B](as: List[A])(f: A=>List[B]): List[B] = {
  concat(map(as)(f))
}

def filter[A](as: List[A])(f: A=>Boolean): List[A] = {
  flatMap(as)((a) => f(a) match {
    case true => List(a)
    case false => Nil
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