package codes.quine.labo
package neko
package props

import laws._

import scalaprops._

trait SemigroupProps[A] {
  val laws: SemigroupLaws[A]

  def semigroupAssociativity(implicit ga: Gen[A], ea: Eq[A]): Property =
    Property.forAll(laws.semigroupAssociativity(_: A, _: A, _: A))

  def semigroup(implicit ga: Gen[A], ea: Eq[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.semigroup)(
      NekoLaw.semigroupAssociativity -> semigroupAssociativity
    )
}

object SemigroupProps {
  def apply[A: Semigroup]: SemigroupProps[A] = new SemigroupProps[A] {
    val laws: SemigroupLaws[A] = SemigroupLaws[A]
  }
}
