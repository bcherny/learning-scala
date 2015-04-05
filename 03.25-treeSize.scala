sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + size(l) + size(r)
}

val l = Leaf(42)
val a = Branch(l, l)
val b = Branch(l, l)
val c = Branch(a, b)

assert(size(a) == 3)
assert(size(b) == 3)
assert(size(c) == 7)