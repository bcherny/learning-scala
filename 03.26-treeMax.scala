sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def max(t: Tree[Int]): Int = t match {
  case Leaf(v) => v
  case Branch(l,r) => max(l) max max(r)
}

val a = Branch(Leaf(42), Leaf(21))
val b = Branch(Leaf(2), Leaf(10))
val c = Branch(a, b)

assert(max(a) == 42)
assert(max(b) == 10)
assert(max(c) == 42)