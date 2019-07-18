package codes.quine.labo.neko
package props

import laws._, instances._

import scalaprops._

trait EqProps[A] {
  val laws: EqLaws[A]
  import laws._ // to use Eq[A] instance

  def eqReflectivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.eqReflexivity(_: A))

  def eqSymmetry(implicit ga: Gen[A]): Property =
    Property.forAll(laws.eqSymmetry(_: A, _: A))

  def eqAntiSymmetry(implicit ga: Gen[A], gf: Gen[A => A]): Property =
    Property.forAll(laws.eqAntiSymmetry(_: A, _: A, _: A => A))

  def eqTransivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.eqTransivity(_: A, _: A, _: A))

  def eq(implicit ga: Gen[A], gf: Gen[A => A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.eq)(
      NekoLaw.eqReflexivity -> eqReflectivity,
      NekoLaw.eqSymmetry -> eqSymmetry,
      NekoLaw.eqAntiSymmetry -> eqAntiSymmetry,
      NekoLaw.eqTransivity -> eqTransivity
    )
}

object EqProps {
  def apply[A](implicit instance: Eq[A]): EqProps[A] = new EqProps[A] {
    val laws = EqLaws[A]
  }
}
