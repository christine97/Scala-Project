

sealed trait RE
case object Eps extends RE
case object Phi extends RE
case class Lit(c:Char) extends RE
case class Star(re:RE) extends RE
case class Concat(re1:RE, re2:RE) extends RE
case class Choice(re1:RE, re2:RE) extends RE


def pp(re:RE):String = re match
{
  case Eps             => "()"
  case Phi             => "Phi"
  case Lit(c)          => c.toString
  case Star(re1)       => pp(re1) ++ "*"
  case Concat(re1,re2) => pp(re1) ++ pp(re2)
  case Choice(re1,re2) => pp(re1) ++ "+" ++ pp(re2)
}


def epsIn(re:RE):Boolean = re match {
  case Eps             => true
  case Star(_)         => true
  case Phi             => false
  case Lit(_)          => false
  case Choice(re1,re2) => epsIn(re1) || epsIn(re2)
  case Concat(re1,re2) => epsIn(re1) && epsIn(re2)
}


def deriv(re:RE, c:Char):RE = re match {

  case Eps                           => Phi // ()  / c     ---> Phi
  case Phi                           => Phi // Phi / c     ---> Phi
  case Lit(c1) if c1 == c            => Eps // c'  / c     ---> ()    //if c' == c
  case Lit(c1)                       => Phi // c'  / c     ---> Phi   //if c' != c
  case Star(re1)                     => Concat(deriv(re1,c),re) // re* / c     ---> (re/c) re*  //(note, because re* == (()+(re re*)) // because re == Star(re1)
  case Choice(re1,re2)               => Choice(deriv(re1,c),deriv(re2,c)) // re1+re2 / c ---> (re1/c)+(re2/c)
  case Concat(re1,re2) if epsIn(re1) => Choice(Concat(deriv(re1,c),re2),deriv(re2,c)) // re1 re2 / c  ---> (re1/c)re2 + re2/c     if () in re1
  case Concat(re1,re2)               => Concat(deriv(re1,c),re2)// re1 re2 / c  ---> (re1/c)re2  // if () not in re1
}

def matchWith(s:List[Char],re:RE): Boolean = s match {

  case Nil => epsIn(re) // s is "" // 1) s is "" and epsIn(re)

  case (c::s1) => matchWith(s1, deriv(re, c)) // 2) s is (c s1) (e.g., s == "hello", c is 'h', s1 is "ello") and (s1 can be matched with re/c)
  case (c::s2) => matchWith(s2, deriv(re, c))
}

// Day 7 - Homework

/*
s = "hello"
re = (h+e+l+o)*

s matched re  --->
"hello" matched (h+e+l+o)* --->
"ello" matched ((h+e+l+o)*)/h --->
"ello" matched ()(h+e+l+o)* ---> (simp)
"ello" matched (h+e+l+o)* --->
"llo" matched  ((h+e+l+o)*)/e --->
"llo" matched  (h+e+l+o)* --->
...
"o" matched ((h+e+l+o)*)/h -->
"o" matched (h+e+l+o)* -->
"" matched ((h+e+l+o)*)/o --->
"" matched (h+e+l+o)* --->
() in (h+e+l+o)* ---> true
*/

  val s = "hello".toList
  val re_h = Lit('h')
  val re_e = Lit('e')
  val re_l = Lit('l')
  val re_o = Lit('o')
  val re = Star(Choice(re_h,Choice(re_e,Choice(re_l,re_o))))

// test code
matchWith(s,re)


/*
// TODO #1, translate the following math into scala test example
s1 = "hello"
re1 = hell(o+i) // this is the regular expression he wanted
"hello" matched hell(o+i) --->
"ello" matched hell(o+i)/h --->
"ello" matched ell(o+i) --->
"llo" matched ell(o+i)/e --->
"llo" matched ll(o+i) --->
"lo" matched ll(o+i)/l --->
"lo" matched l(o+i) --->
"o" matched l(o+i)/l --->
"o" matched (o+i) --->
"" matched (o+i)/o --->
      (o+i)/o ---> (o/o + i/o) ---> (() + Phi)
"" matched ()+Phi --->
"" matched () --->
() in () ---> true
*/

// TODO #1, translate the following math into scala test example

  val s1 ="hello".toList
  val re1_h = Lit('h')
  val re1_e = Lit('e')
  val re1_l = Lit('l')
  val re1_o = Lit('o')
  val re1_i = Lit('i')
  val re1= Concat(re1_h, Concat(re1_e, Concat(re1_l, Concat(re1_l, Choice(re1_o, re1_i)))))

//test code
matchWith(s1,re1)




// TODO #2, translate the following math into scala test example
/*
  s2 = "abaac"
  re2 = ((ab+a)(baa+a))(c+ac)
  */

  val s2 = "abaac".toList
  val re2_a = Lit('a')
  val re2_b = Lit('b')
  val re2_c = Lit('c')
  //val re2 = Choice(Concat(re2_a,re2_b), re2_a)              // (ab+a)
  //val re2 = Choice(Concat(re2_b, Star(re2_a)) ,  re2_a)     // (baa+a)
  //val re2 = Choice(re2_c, Concat(re2_a, re2_c))             // (c+ac)
  val re2 = Concat(Concat(Choice(Concat(re2_a,re2_b), re2_a), Choice(Concat(re2_b, Star(re2_a)) ,  re2_a)), Choice(re2_c, Concat(re2_a, re2_c)))

  // test code
  matchWith(s2, re2)



// TODO #3, apply simplification in the matchWith method, in particular at the location where deriv() is applied.
/*
def matchWith(s:List[Char],re:RE): Boolean = s match {

  case Nil => epsIn(re) // s is "" // 1) s is "" and epsIn(re)

  case (c::s1) => matchWith(s1, deriv(re, c)) // 2) s is (c s1) (e.g., s == "hello", c is 'h', s1 is "ello") and (s1 can be matched with re/c)

case (c::s2) => matchWith(s2, deriv(re2, c2)
}
 */

