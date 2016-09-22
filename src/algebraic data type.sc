import com.sun.java.swing.plaf.windows.WindowsTreeUI.ExpandedIcon

 // 1st - sealed trait Exp
sealed trait Exp

 // 2nd - case class ..... extends
case class Val(v:Int) extends Exp
case class Plus(e1:Exp, e2:Exp) extends Exp
case class Minus (e1:Exp, e2:Exp) extends Exp
case class Divide (e1:Exp, e2:Exp) extends Exp


 // 3rd - Define (def)
def simp(e:Exp): Exp = e match {
  case Val(v) => e
  case Plus(Val(0), e2) => simp(e2)
  case Plus(e1, e2) => Plus(simp(e1), simp(e2))
  case Divide (Val(0), e2) => Divide (Val(0), simp(e2))
}


  // Testing
Plus(Val(0), Val(2))
Minus (Val(3), Val(1))
Divide (Val(3), Val(3))
Divide (Val(0), Val(3)) // ERROR