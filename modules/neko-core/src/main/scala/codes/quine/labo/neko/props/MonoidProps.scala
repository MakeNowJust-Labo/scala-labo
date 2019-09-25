package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait MonoidProps[A] {
  val laws: MonoidLaws[A]
  import laws._

  def monoidLeftIdentity(implicit ga: Gen[A], a: Eq[A]): Property =
    Property.forAll(laws.monoidLeftIdentity(_: A))

  def monoidRightIdentity(implicit ga: Gen[A], a: Eq[A]): Property =
    Property.forAll(laws.monoidRightIdentity(_: A))

  def props(implicit ga: Gen[A], ea: Eq[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.monoid)(
      NekoLaw.monoidLeftIdentity -> monoidLeftIdentity,
      NekoLaw.monoidRightIdentity -> monoidRightIdentity
    )

  def all(implicit ga: Gen[A], ea: Eq[A]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.monoidAll, SemigroupProps[A].all, props)
}

object MonoidProps {
  def apply[A: Monoid]: MonoidProps[A] = new MonoidProps[A] {
    val laws: MonoidLaws[A] = MonoidLaws[A]
  }
}
