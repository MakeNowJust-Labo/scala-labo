package codes.quine.labo.nyan.props

trait ExhaustiveCheck[A] {
  def values: Set[A]
}

object ExhaustiveCheck {
  def apply[A: ExhaustiveCheck]: ExhaustiveCheck[A] = implicitly

  implicit object BooleanExhaustiveCheck extends ExhaustiveCheck[Boolean] {
    val values: Set[Boolean] = Set(true, false)
  }

  implicit def SetExhaustiveCheck[A: ExhaustiveCheck]: ExhaustiveCheck[Set[A]] = new ExhaustiveCheck[Set[A]] {
    lazy val values: Set[Set[A]] = ExhaustiveCheck[A].values.subsets.toSet
  }

  implicit def Tuple2ExhaustiveCheck[A: ExhaustiveCheck, B: ExhaustiveCheck]: ExhaustiveCheck[(A, B)] =
    new ExhaustiveCheck[(A, B)] {
      lazy val values: Set[(A, B)] = for {
        a <- ExhaustiveCheck[A].values
        b <- ExhaustiveCheck[B].values
      } yield (a, b)
    }
}
