def init[A](l: List[A]): List[A] = {

  def loop[A](l: List[A], a: List[A]): List[A] = l match {

    case Nil => Nil
    case h :: Nil => a.reverse
    case h :: t => loop(t, h :: a)

  }

  loop(l, List())

}

assert(
  init(List(1,2,3,4)),
  List(1,2,3)
)

assert(
  init(List(1)),
  List()
)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}
