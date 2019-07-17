package codes.quine.labo.nyan
package data

import props._, instances._

import scalaprops._

object EvalProps extends Scalaprops {
  implicit def EvalGen[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def EvalCogen[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  val laws =
    Properties.list(
      EqProps[Eval[Int]].eq,
      FunctorProps[Eval].functor[MinInt, MinInt, MinInt],
      ApplicativeProps[Eval].applicative[MinInt, MinInt, MinInt],
      MonadProps[Eval].monad[MinInt, MinInt, MinInt]
    )
}
