/* regular expression
.*(codebunk).*

"" match with ()
Phi means empty language, no string can ever match Phi
"a" matches with a
"aaaaaa" matches with a*  because for all "a", "a" matches with a
"ab" matches with ab because ab is a concatenation, where "a" matches a and "b" matches b
"ab" matches with (ab)+c because "ab" matches with ab
"c" matches with (ab)+c because "c" matches with c

re ::= ()    // empty string or epsilon
     | Phi   // empty set or empty language
     | c     // character
     | re*   // 0 or more repetition
     | re re // concatenation
     | re+re // choice
*/

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

// re = ab+c
val re = Choice(Concat(Lit('a'), Lit('b')) ,Lit('c'))

pp(re)



/* regular expression derivative

re / c is defined as the regular expression obtained by removing the leading character c from re. (English definition)

(math definition)

()  / c     ---> Phi
Phi / c     ---> Phi
c'  / c     ---> ()                     if c' == c
c'  / c     ---> Phi                    otherwise
re* / c     ---> (re/c)re*             (note, because re* == (()+(re re*)))
re1+re2 / c ---> (re1/c) + (re2/c)
re1re2 / c  ---> (re1/c)re2             if () not in re1
re1re2 / c  ---> (re1/c)re2 + re2/c     if () in re1


e.g. for kleene's star
a* is matching with "", "a", "aa", ....
a* == () + aa* because
() is matching with ""
aa* is matching with  "a", "aa", ....

if we compute a* / a
1st ---> (a/a)a*
2nd ---> ()a*
3rd ----> a*

*/



/*
checking for epsilon containment
() in ()

() in re*

() not in Phi

() not in c

() in re1+re2 if (() in re1) || (() in re2)

() in re1re2  if (() in re1) && (() in re2)
*/

def epsIn(re:RE):Boolean = re match {
  case Eps             => true
  case Star(_)         => true
  case Phi             => false
  case Lit(_)          => false
  case Choice(re1,re2) => epsIn(re1) || epsIn(re2)
  case Concat(re1,re2) => epsIn(re1) && epsIn(re2)
}

def deriv(re:RE, c:Char):RE = re match {
  // ()  / c     ---> Phi
  case Eps => Phi

  // Phi / c     ---> Phi
  case Phi => Phi

  // c'  / c     ---> ()                     if c' == c
  case Lit(c1) if c1 == c => Eps

  // c'  / c     ---> Phi                    otherwise
  case Lit(c1)            => Phi

  // re* / c     ---> (re/c) re*             (note, because re* == (()+(re re*))
  case Star(re1)          => Concat(deriv(re1,c),re) // because re == Star(re1)

  // re1+re2 / c ---> (re1/c)+(re2/c)
  case Choice(re1,re2)    => Choice(deriv(re1,c),deriv(re2,c))

  // re1 re2 / c  ---> (re1/c)re2 + re2/c     if () in re1
  case Concat(re1,re2) if epsIn(re1) => Choice(Concat(deriv(re1,c),re2),deriv(re2,c))

  // re1 re2 / c  ---> (re1/c)re2             if () not in re1
  case Concat(re1,re2)               => Concat(deriv(re1,c),re2)
}

/*
scala> deriv(Star(Lit('a')),'a')
res13: RE = Concat(Eps,Star(Lit(a)))

scala> pp(deriv(Star(Lit('a')),'a'))
res14: String = ()a*
*/

// TODO #1: define a simplification function RE
/*
()re ---> re
re() ---> re
Phi re ---> Phi
re Phi  ---> Phi
Phi+re ---> re
re+Phi ---> re
re+re ---> re
()* ---> ()
Phi* ---> ()
*/

def simp(re:RE):RE = re match {
  case Concat(Eps, re)         => re
  case Concat(re, Eps)         => re
  case Concat (Phi, re)        => Phi
  case Concat (re, Phi)        => Phi
  case Choice(Phi, re)         => Choice(simp(Phi), simp(re))
  case Choice(re, Phi)         => Choice(simp(re), simp(Phi))
  case (re1, re2)              => Choice(simp(re1), simp(re2))
  case Concat(Eps, Star(re))   => Eps
  case Concat(Phi, Star(re))   => Eps
}



// TODO #2: define a regular expression matching function
/*
s to be a string or a List[Char]

a string s can be matched with a regular expression re,  iff,
   1) s is "" and epsIn(re) or
   2) s is (c s1) (e.g., s == "hello", c is 'h', s1 is "ello") and (s1 can be matched with re/c)
   Hints: you need to make use of the deriv() method defined above
*/

def matchWith(s:List[Char],re:RE): Boolean = s match {

  case Nil => epsIn(re) // s is "" // 1) s is "" and epsIn(re)

  case (c::s1) => matchWith(s1, deriv(re, c)) // 2) s is (c s1) (e.g., s == "hello", c is 'h', s1 is "ello") and (s1 can be matched with re/c)

}