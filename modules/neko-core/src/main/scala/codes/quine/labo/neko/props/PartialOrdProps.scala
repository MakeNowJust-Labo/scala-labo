package codes.quine.labo
package neko
package props

import scalaprops._
import instances._, laws._

trait PartialOrdProps[A] {
  val laws: PartialOrdLaws[A]
  import laws._

  def partialOrdReflexivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.partialOrdReflexivity(_: A))

  def partialOrdAntisymmetry(implicit ga: Gen[A]): Property =
    Property.forAll(laws.partialOrdAntisymmetry(_: A, _: A))

  def partialOrdTransivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.partialOrdTransivity(_: A, _: A, _: A))

  def props(implicit ga: Gen[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.partialOrd)(
      NekoLaw.partialOrdReflexivity -> partialOrdReflexivity,
      NekoLaw.partialOrdAntisymmetry -> partialOrdAntisymmetry,
      NekoLaw.partialOrdTransivity -> partialOrdTransivity
    )

  def all(implicit ga: Gen[A], gf: Gen[A => A]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.partialOrdAll, EqProps[A].all, props)
}

object PartialOrdProps {
  def apply[A: PartialOrd]: PartialOrdProps[A] = new PartialOrdProps[A] {
    val laws: PartialOrdLaws[A] = PartialOrdLaws[A]
  }
}
