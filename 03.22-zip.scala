
def zip (as: List[Int], bs: List[Int]) = {

  def loop (as: List[Int], bs: List[Int], acc: List[Int]): List[Int] = (as,bs) match {
    case (Nil, _) => acc
    case (_, Nil) => acc
    case (a::as, b::bs) => loop(as, bs, acc :+ (a + b))
  }

  loop(as, bs, Nil:List[Int])

}

assert(zip(List(), List()), List())
assert(zip(List(1,2,3), List(4,5,6)), List(5,7,9))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}