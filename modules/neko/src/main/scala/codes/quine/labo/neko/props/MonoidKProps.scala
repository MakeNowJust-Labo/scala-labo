package codes.quine.labo.neko
package props

import laws._

import scalaprops._

trait MonoidKProps[F[_]] {
  val laws: MonoidKLaws[F]

  implicit val F: MonoidK[F] = laws.F

  def monoidKLeftIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monoidKLeftIdentity(_: F[A]))

  def monoidKRightIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monoidKRightIdentity(_: F[A]))

  def monoidKAssociativity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monoidKAssociativity(_: F[A], _: F[A], _: F[A]))

  def monoidK[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.monoidK)(
      NekoLaw.monoidKLeftIdentity -> monoidKLeftIdentity,
      NekoLaw.monoidKRightIdentity -> monoidKRightIdentity,
      NekoLaw.monoidKAssociativity -> monoidKAssociativity
    )
}

object MonoidKProps {
  def apply[F[_]](implicit instance: MonoidK[F]): MonoidKProps[F] = new MonoidKProps[F] {
    override lazy val laws: MonoidKLaws[F] = MonoidKLaws[F](instance)
  }
}
