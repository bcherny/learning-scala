sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(v) => 0
  case Branch(l,r) => 1 + (depth(l) max depth(r))
}

val a = Branch(Leaf(42), Leaf(21))
val b = Branch(Leaf(2), Leaf(10))
val c = Branch(a, b)
val d = Branch(c, Leaf(3))

assert(depth(a), 1)
assert(depth(b), 1)
assert(depth(c), 2)
assert(depth(d), 3)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}