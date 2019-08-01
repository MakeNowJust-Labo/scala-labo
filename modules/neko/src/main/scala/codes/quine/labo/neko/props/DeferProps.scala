package codes.quine.labo
package neko
package props

import laws._, instances._

import scalaprops._

trait DeferProps[F[_]] {
  val laws: DeferLaws[F]

  def deferIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.deferIdentity(_: F[A]))

  def deferDoesNotEvaluate[A](implicit gfa: Gen[Unit => F[A]]): Property =
    Property.forAll(laws.deferDoesNotEvaluate(_: Unit => F[A]))

  def deferStackSafety[A](implicit gfa: Gen[Unit => F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.deferStackSafety(_: Unit => F[A]))

  def defer[A](implicit gfa: Gen[F[A]], gufa: Gen[Unit => F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.defer)(
      NekoLaw.deferIdentity -> deferIdentity[A],
      NekoLaw.deferDoesNotEvaluate -> deferDoesNotEvaluate[A],
      NekoLaw.deferStackSafety -> deferStackSafety[A]
    )
}

object DeferProps {
  def apply[F[_]: Defer]: DeferProps[F] = new DeferProps[F] {
    val laws: DeferLaws[F] = DeferLaws[F]
  }
}
