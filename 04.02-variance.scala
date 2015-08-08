def mean (xs: Seq[Double]): Option[Double] = xs.size match {
	case 0 => None
	case _ => Option(xs.sum/xs.size)
}

def variance (xs: Seq[Double]): Option[Double] = {
	mean(xs) flatMap(m => mean(xs map(x => math.pow(x - m, 2))))
}

assert(variance(Seq(1,2,3)), Some(2.0/3))
assert(variance(Seq()), None)

def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}