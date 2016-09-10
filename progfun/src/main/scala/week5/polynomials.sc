object polynomials {

  //  class Poly(val terms: Map[Int, Double]) {
  //    def +(other: Poly) =
  //      new Poly(terms ++ (other.terms map adjust))
  //
  //    def adjust(term: (Int, Double)) = {
  //      val (exp, coef) = term
  //      terms.get(exp) match {
  //        case None => term
  //        case Some(currentCoef) => exp -> (currentCoef + coef)
  //      }
  //    }
  //
  //    override def toString =
  //      terms
  //        .toList
  //        .sortBy { case (exp, _) => -exp }
  //        .map {
  //          case (0, coef) => coef
  //          case (exp, coef) => s"${coef} * x^${exp}"
  //        }
  //        .mkString(" + ")
  //  }

  class Poly(termsInput: Map[Int, Double]) {
    val terms = termsInput withDefaultValue 0.0

    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    //    def +(other: Poly) = {
    //      def adjust(term: (Int, Double)) = {
    //        val (exp, coef) = term
    //        exp -> (coef + terms(exp))
    //      }
    //
    //      new Poly(terms ++ (other.terms map adjust))
    //    }

    def +(other: Poly) = {
      def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
        val (exp, coef) = term
        terms.updated(exp, coef + terms(exp))
      }

      new Poly(other.terms.foldLeft(terms)(addTerm))
    }

    override def toString =
      terms
        .toList
        .sortBy { case (exp, _) => -exp }
        .map {
          case (0, coef) => coef
          case (exp, coef) => s"${coef} * x^${exp}"
        }
        .mkString(" + ")
  }

  val x = new Poly(0 -> 1, 1 -> 2)
  val y = new Poly(0 -> 2, 2 -> 3)
  x + y

  val map = Map(0 -> 1, 1 -> 2)
  map.transform { case (k, v) => v + 1 }
  map.filter { case (k, v) => v % 2 == 0 }
}