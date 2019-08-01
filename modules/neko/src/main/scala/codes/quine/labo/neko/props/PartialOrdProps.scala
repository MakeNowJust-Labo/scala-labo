package codes.quine.labo
package neko
package props

import laws._, instances._

import scalaprops._

trait PartialOrdProps[A] extends EqProps[A] {
  val laws: PartialOrdLaws[A]

  def partialOrdReflexivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.partialOrdReflexivity(_: A))

  def partialOrdAntisymmetry(implicit ga: Gen[A]): Property =
    Property.forAll(laws.partialOrdAntisymmetry(_: A, _: A))

  def partialOrdTransivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.partialOrdTransivity(_: A, _: A, _: A))

  def partialOrd(implicit ga: Gen[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.partialOrd)(
      NekoLaw.partialOrdReflexivity -> partialOrdReflexivity,
      NekoLaw.partialOrdAntisymmetry -> partialOrdAntisymmetry,
      NekoLaw.partialOrdTransivity -> partialOrdTransivity
    )
}

object PartialOrdProps {
  def apply[A: PartialOrd]: PartialOrdProps[A] = new PartialOrdProps[A] {
    val laws: PartialOrdLaws[A] = PartialOrdLaws[A]
  }
}
