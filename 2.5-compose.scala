def compose[A,B,C](f: B => C, g: A => B): A => C = {
  (a: A) => f(g(a))
}

def square = (a: Int) => a*a
def double = (a: Int) => a*2

assert(compose(square, double)(2), 16)
assert(compose(square, double)(3), 36)
assert(compose(square, double)(4), 64)

def assert(a: Any, b: Any) { 
  if (a == b)
    println("assertion successful")
  else
    println("assertion failed") 
}