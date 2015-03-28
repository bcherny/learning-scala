
def toString (as: List[Double]): List[String] = {

  as.foldLeft(Nil:List[String])((a,b) => a :+ b.toString)

}

assert(toString(List()), List())
assert(toString(List(1.1, 2.2, 3.3)), List("1.1", "2.2", "3.3"))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}