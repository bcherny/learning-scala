

// def hasSubsequence [A] (sup: List[A], sub: List[A]): Any = {

//   // get index of first match, if there is one
//   def getFirstIndex (sup: List[A], sub: List[A]): Any = {
//     sup
//       .zipWithIndex
//       .filter(_._1 == sub.head)
//       .map(_._2) match {
//         case h::Nil => h
//         case _ => Nil
//       }
//   }

//   val index = getFirstIndex(sup, sub)

//   if index
//     sup.zipWithIndex.map (s,i) => {
//       if i >
//     }

// }

// from https://github.com/fpinscala/fpinscala/blob/master/answerkey/datastructures/24.answer.scala
@annotation.tailrec
def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
  case (_,Nil) => true
  case (h::t,h2::t2) if h == h2 => startsWith(t, t2)
  case _ => false
}

@annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => sub == Nil
  case _ if startsWith(sup, sub) => true
  case _::t => hasSubsequence(t, sub)
}


assert(hasSubsequence(List(1,2,3,4), List(1,2)), true)
assert(hasSubsequence(List(1,2,3,4), List(3,4)), true)
assert(hasSubsequence(List(1,2,3,4), List(1,2,3,4)), true)
assert(hasSubsequence(List(1,2,3,4), List(2,3,4)), true)
assert(hasSubsequence(List(1,2,3,4), List(3,2)), false)
assert(hasSubsequence(List(1,2,3,4), List(5,4,3,2,1)), false)
assert(hasSubsequence(List(1,2,3,4), List(1,2,4)), false)
assert(hasSubsequence(List(1,2,3,4), List(6)), false)


def assert(a: Any, b: Any) {
  if (a == b)
    println(s"assertion successful - $a equals $b")
  else
    println(s"assertion failed - expected $a to equal $b")
}