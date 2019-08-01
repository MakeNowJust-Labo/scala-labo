package codes.quine.labo
package neko
package props

import laws._

import scalaprops._

trait MonoidProps[A] extends SemigroupProps[A] {
  val laws: MonoidLaws[A]

  def monoidLeftIdentity(implicit ga: Gen[A], a: Eq[A]): Property =
    Property.forAll(laws.monoidLeftIdentity(_: A))

  def monoidRightIdentity(implicit ga: Gen[A], a: Eq[A]): Property =
    Property.forAll(laws.monoidRightIdentity(_: A))

  def monoid(implicit ga: Gen[A], ea: Eq[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.monoid)(
      NekoLaw.monoidLeftIdentity -> monoidLeftIdentity,
      NekoLaw.monoidRightIdentity -> monoidRightIdentity
    )
}

object MonoidProps {
  def apply[A: Monoid]: MonoidProps[A] = new MonoidProps[A] {
    val laws: MonoidLaws[A] = MonoidLaws[A]
  }
}
