package codes.quine.labo
package neko
package props

import laws._

import scalaprops._

trait SemigroupKProps[F[_]] {
  val laws: SemigroupKLaws[F]

  def semigroupKAssociativity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.semigroupKAssociativity(_: F[A], _: F[A], _: F[A]))

  def semigroupK[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.semigroupK)(
      NekoLaw.semigroupKAssociativity -> semigroupKAssociativity[A]
    )
}

object SemigroupKProps {
  def apply[F[_]: SemigroupK]: SemigroupKProps[F] = new SemigroupKProps[F] {
    val laws: SemigroupKLaws[F] = SemigroupKLaws[F]
  }
}
