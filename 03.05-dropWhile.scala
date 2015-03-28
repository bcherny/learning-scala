def dropWhile [A] (l: List[A], f: A => Boolean): List[A] = {

  if (f(l.head) == true) {
    dropWhile(l.tail, f)
  } else {
    l
  }

}

assert(
  dropWhile(List(1,2,3,4), (n:Int) => n < 3),
  List(3,4)
)

assert(
  dropWhile(List(2,4,6,8,9), (n:Int) => n % 2 == 0),
  List(9)
)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}
