def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
	a flatMap(_a => b map(_b => f(_a, _b)))

val a = Some(1)
val b = Some(2)
val c = None
def fn (a: Int, b: Int): Int = a + b

assert(map2(a,b)(fn), Some(3))
assert(map2(a,c)(fn), None)
assert(map2(b,c)(fn), None)
assert(map2(c,c)(fn), None)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}