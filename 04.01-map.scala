/*
	
	INCOMPLETE IMPLEMENTATION
	TODO: revisit

*/


// def map[B](f: A => B): Option[B] = this match {
//   case None => None
//   case Some(a) => Some(f(a))
// }
def getOrElse[B>:A](default: => B): B = this match {
  case None => default
  case Some(a) => a
}
// def flatMap[B](f: A => Option[B]): Option[B] = this match {
// 	case Some(a) => f(a)
// 	case None => None
// }

val o = Option(2)

assert(getOrElse(o), Option(2))

// assert(p.map(_*2), Some(4))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}