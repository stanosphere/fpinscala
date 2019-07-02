abstract class Term
case class Var(name: String) extends Term
case class Fun(arg: String, body: Term) extends Term