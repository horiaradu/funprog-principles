object patterns {

  trait Expr {
    def eval: Int =
      this match {
        case Number(n) => n
        case Sum(e1, e2) => e1.eval + e2.eval
        case Prod(e1, e2) => e1.eval * e2.eval
      }

    def show: String =
      this match {
        case Number(n) => n.toString
        case Sum(e1, e2) => s"${e1.show} + ${e2.show}"
        case Prod(e1, e2) => s"(${e1.show} * ${e2.show})"
      }
  }

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  val expr = Sum(Prod(Number(1), Number(2)), Number(10))
  expr.eval
  expr.show

}