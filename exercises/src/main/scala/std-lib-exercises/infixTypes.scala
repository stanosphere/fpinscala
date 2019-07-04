case class Person(name: String)
class Loves[A, B](val a: A, val b: B)

def announceCouple(couple: Person Loves Person) =
//Notice our type: Person loves Person!
  couple.a.name + " is in love with " + couple.b.name

val romeo = new Person("Romeo")
val juliet = new Person("Juliet")

val starCrossed = announceCouple(new Loves(romeo, juliet))