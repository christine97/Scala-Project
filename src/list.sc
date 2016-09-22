
// 1st - sealed trait list
sealed trait List[A]

// 2nd - case class .... extends
case class Nil[A]() extends List[A]
case class Cons[A](x:A , xs:List[A]) extends List[A]

//

val x = Cons(1, Cons(2, Nil()))

// 3rd - Define (def)
def length[A](l:List[A]):Int = l match {
  case Nil() => 0
  case Cons(_, xs) => 1 + length(xs)
}
--------------------------------------------------------
/*

 // mixing subtying with parametric polymorphism

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](x:A, xs:List[A]) extends List[A]

val x = Cons(1,Cons(2, Nil))

def length[A]l:List[A]: Int = l match {
  case Nil => 0
  case Cons(_,xs) => 1 + length(xs)
}
 */



