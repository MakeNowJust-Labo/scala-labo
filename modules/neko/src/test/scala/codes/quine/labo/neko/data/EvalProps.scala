package codes.quine.labo
package neko
package data

import props._, instances._

import scalaprops._

object EvalProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def EvalGen[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def EvalCogen[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  val laws =
    Properties.list(
      EqProps[Eval[Int]].eq,
      PartialOrdProps[Eval[Int]].partialOrd,
      OrdProps[Eval[Int]].ord,
      HashProps[Eval[Int]].hash,
      SemigroupProps[Eval[String]].semigroup,
      MonoidProps[Eval[String]].monoid,
      FunctorProps[Eval].functor[MinInt, MinInt, MinInt],
      ApplicativeProps[Eval].applicative[MinInt, MinInt, MinInt],
      MonadProps[Eval].monad[MinInt, MinInt, MinInt],
      DeferProps[Eval].defer[Int]
    )
}
