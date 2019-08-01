package codes.quine.labo
package neko
package props

import laws._

import scalaprops._

trait MonoidKProps[F[_]] {
  val laws: MonoidKLaws[F]

  def monoidKLeftIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monoidKLeftIdentity(_: F[A]))

  def monoidKRightIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monoidKRightIdentity(_: F[A]))

  def monoidK[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.monoidK)(
      NekoLaw.monoidKLeftIdentity -> monoidKLeftIdentity,
      NekoLaw.monoidKRightIdentity -> monoidKRightIdentity
    )
}

object MonoidKProps {
  def apply[F[_]: MonoidK]: MonoidKProps[F] = new MonoidKProps[F] {
    val laws: MonoidKLaws[F] = MonoidKLaws[F]
  }
}
