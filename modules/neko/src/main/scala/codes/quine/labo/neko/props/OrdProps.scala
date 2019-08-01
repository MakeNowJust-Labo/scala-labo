package codes.quine.labo
package neko
package props

import laws._, instances._

import scalaprops._

trait OrdProps[A] extends PartialOrdProps[A] {
  val laws: OrdLaws[A]

  def ordTotality(implicit ga: Gen[A]): Property =
    Property.forAll(laws.ordTotality(_: A, _: A))

  def ord(implicit ga: Gen[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.ord)(
      NekoLaw.ordTotality -> ordTotality
    )
}

object OrdProps {
  def apply[A: Ord]: OrdProps[A] = new OrdProps[A] {
    val laws: OrdLaws[A] = OrdLaws[A]
  }
}
