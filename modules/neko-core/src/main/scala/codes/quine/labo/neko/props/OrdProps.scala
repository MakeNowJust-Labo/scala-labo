package codes.quine.labo
package neko
package props

import scalaprops._
import instances._, laws._

trait OrdProps[A] {
  val laws: OrdLaws[A]
  import laws._

  def ordTotality(implicit ga: Gen[A]): Property =
    Property.forAll(laws.ordTotality(_: A, _: A))

  def props(implicit ga: Gen[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.ord)(
      NekoLaw.ordTotality -> ordTotality
    )

  def all(implicit ga: Gen[A], gf: Gen[A => A]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.ordAll, PartialOrdProps[A].all, props)
}

object OrdProps {
  def apply[A: Ord]: OrdProps[A] = new OrdProps[A] {
    val laws: OrdLaws[A] = OrdLaws[A]
  }
}
