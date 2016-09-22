
class Person(n:String,i:String) {
  private val name:String = n
  private val id:String   = i
  def getName():String = name
  def getId():String = id
}

trait NightOwl {
  def stayUpLate():Unit
}

class Student(n:String, i:String, g:Double) extends Person(n,i) with NightOwl {
  private var gpa = g
  def getGPA() = gpa
  def setGPA(g:Double) =
  {
    gpa = g
  }
  override def stayUpLate():Unit =
  {
    println("woohoo")
  }
}

class Staff(n:String, i:String, sal:Double) extends Person(n,i) {
  private var salary = sal
  def getSalary() = salary
  def setSalary(sal:Double) =
  {
    salary = sal
  }
}

// /* we test this out in cmd prompt
val tom = new Student("Tom", "X1235", 4.0) // Setter method
val jerry = new Staff("Jerry", "T0001", 500000.0)
val christine = new Student("Christine", "S001", 2002.0)



//set variable kk
val kk = new Student("kk", "kk", 4.0)

val gif = new Staff("Gif", "123", 2200)

//get variable kk
kk.getGPA()
gif.getSalary()


