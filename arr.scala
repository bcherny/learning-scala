object Arr {
  
  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A,A)=>Boolean):Boolean = {

    if (as.length > 1 && !ordered(as(0), as(1))) false
    else if (as.length == 1) true
    else isSorted(as.drop(1), ordered)

  }

}
val comparator = (a: Int, b: Int) => a < b

assert(Arr.isSorted(Array(1), comparator), true)
assert(Arr.isSorted(Array(1,2,3), comparator), true)
assert(Arr.isSorted(Array(1,4,2,3), comparator), false)
assert(Arr.isSorted(Array(3,2,1), comparator), false)

def assert(a: Boolean, b: Boolean) { 
  if (a == b)
    println("assertion successful")
  else
    println("assertion failed") 
} 
