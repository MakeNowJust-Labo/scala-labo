package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait SemigroupProps[A] {
  val laws: SemigroupLaws[A]

  def semigroupAssociativity(implicit ga: Gen[A], ea: Eq[A]): Property =
    Property.forAll(laws.semigroupAssociativity(_: A, _: A, _: A))

  def props(implicit ga: Gen[A], ea: Eq[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.semigroup)(
      NekoLaw.semigroupAssociativity -> semigroupAssociativity
    )

  def all(implicit ga: Gen[A], eq: Eq[A]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.semigroupAll, props)
}

object SemigroupProps {
  def apply[A: Semigroup]: SemigroupProps[A] = new SemigroupProps[A] {
    val laws: SemigroupLaws[A] = SemigroupLaws[A]
  }
}
