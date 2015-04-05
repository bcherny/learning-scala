sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def fold[A,B](fl: A => B, fb: (Tree[A],Tree[A]) => B)(t: Tree[A]): B = t match {
  case Leaf(v) => fl(v)
  case Branch(l,r) => fb(l,r)
}

val a = Branch(Leaf(42), Leaf(21))
val b = Branch(Leaf(2), Leaf(10))
val c = Branch(a, b)

/*
  size
 */

def size[A](t: Tree[A]): Int = fold(  
  (_:A) => 1,
  (l:Tree[A], r:Tree[A]) => 1 + size(l) + size(r)
)(t)

assert(size(a), 3)
assert(size(b), 3)
assert(size(c), 7)

/*
  max
 */

def max(t: Tree[Int]): Int = fold(
  (l:Int) => l,
  (l:Tree[Int], r:Tree[Int]) => max(l) max max(r)
)(t)

assert(max(a), 42)
assert(max(b), 10)
assert(max(c), 42)

/*
  depth
 */

def depth[A](t: Tree[A]): Int = fold(
  (_:A) => 0,
  (l:Tree[A], r:Tree[A]) => 1 + depth(l) max depth(r)
)(t)

assert(depth(a), 1)
assert(depth(b), 1)
assert(depth(c), 2)

/*
  map
 */

def map[A,B](t: Tree[A], f: A => B): Tree[B] = fold(
  (v:A) => Leaf(f(v)),
  (l:Tree[A], r:Tree[A]) => Branch(map(l,f), map(r,f))
)(t)

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