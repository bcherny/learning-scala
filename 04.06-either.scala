trait Either[+E, +A] {

	case class Left[+E](value: E) extends Either[E, Nothing]
	case class Right[+A](value: A) extends Either[Nothing, A]

	def map[B](f: A => B): Either[E,B] = this match {
		case Right(a) => Right(f(a))
		case Left(e) => Left(e)
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
		case Right(a) => f(a)
		case Left(e) => Left(e)
	}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]) = this match {
		case Right(a) => a
		case Left(_) => b
	}

}

assert(Right(2).right.map(_*2), Right(4))
assert(Right(2).right.flatMap(a => Right(a*2)), Right(4))
assert(Right(2).right.orElse(a => Right(3)), Right(2))
assert(Left(2).left.orElse(a => Right(3)), Right(3))

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}