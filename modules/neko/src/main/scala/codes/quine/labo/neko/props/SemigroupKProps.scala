package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait SemigroupKProps[F[_]] {
  val laws: SemigroupKLaws[F]

  def semigroupKAssociativity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.semigroupKAssociativity(_: F[A], _: F[A], _: F[A]))

  def props[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.semigroupK)(
      NekoLaw.semigroupKAssociativity -> semigroupKAssociativity[A]
    )

  def all[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.semigroupKAll, props[A])
}

object SemigroupKProps {
  def apply[F[_]: SemigroupK]: SemigroupKProps[F] = new SemigroupKProps[F] {
    val laws: SemigroupKLaws[F] = SemigroupKLaws[F]
  }
}
