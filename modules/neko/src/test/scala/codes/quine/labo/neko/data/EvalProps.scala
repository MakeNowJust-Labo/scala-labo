package codes.quine.labo
package neko
package data

import props._, instances._

import scalaprops._

object EvalProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def evalGenInstance[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def evalCogenInstance[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  val laws =
    Properties.list(
      OrdProps[Eval[Int]].all,
      HashProps[Eval[Int]].all,
      MonoidProps[Eval[String]].all,
      MonadProps[Eval].all[Int, Int, Int],
      DeferProps[Eval].all[Int]
    )
}
