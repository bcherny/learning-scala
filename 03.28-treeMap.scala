sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def map[A,B](t: Tree[A], f: A => B): Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Branch(l,r) => Branch(map(l,f), map(r,f))
}

val a = Branch(Leaf(42), Leaf(21))
val b = Branch(Leaf(2), Leaf(10))
val c = Branch(a, b)

assert(
  map(a, (l:Int) => l+1),
  Branch(Leaf(43), Leaf(22))
)
assert(
  map(b, (l:Int) => l/2),
  Branch(Leaf(1), Leaf(5))
)
assert(
  map(c, (l:Int) => l*2),
  Branch(
    Branch(Leaf(84), Leaf(42)),
    Branch(Leaf(4), Leaf(20))
  )
)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}