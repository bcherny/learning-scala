def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

def fn = (a: Int) => (b: Int) => a + b

assert(uncurry(fn)(1,2), 3)

def assert(a: Any, b: Any) { 
  if (a == b)
    println("assertion successful")
  else
    println("assertion failed") 
}