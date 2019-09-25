package codes.quine.labo
package neko
package props

import scalaprops._
import instances._, laws._

trait EqProps[A] {
  val laws: EqLaws[A]
  import laws._

  def eqReflectivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.eqReflexivity(_: A))

  def eqSymmetry(implicit ga: Gen[A]): Property =
    Property.forAll(laws.eqSymmetry(_: A, _: A))

  def eqAntiSymmetry(implicit ga: Gen[A], gf: Gen[A => A]): Property =
    Property.forAll(laws.eqAntiSymmetry(_: A, _: A, _: A => A))

  def eqTransivity(implicit ga: Gen[A]): Property =
    Property.forAll(laws.eqTransivity(_: A, _: A, _: A))

  def props(implicit ga: Gen[A], gf: Gen[A => A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.eq)(
      NekoLaw.eqReflexivity -> eqReflectivity,
      NekoLaw.eqSymmetry -> eqSymmetry,
      NekoLaw.eqAntiSymmetry -> eqAntiSymmetry,
      NekoLaw.eqTransivity -> eqTransivity
    )

  def all(implicit ga: Gen[A], gf: Gen[A => A]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.eqAll, props)
}

object EqProps {
  def apply[A: Eq]: EqProps[A] = new EqProps[A] {
    val laws = EqLaws[A]
  }
}
