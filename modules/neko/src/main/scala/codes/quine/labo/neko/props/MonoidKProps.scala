package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait MonoidKProps[F[_]] {
  val laws: MonoidKLaws[F]
  import laws._

  def monoidKLeftIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monoidKLeftIdentity(_: F[A]))

  def monoidKRightIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monoidKRightIdentity(_: F[A]))

  def props[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.monoidK)(
      NekoLaw.monoidKLeftIdentity -> monoidKLeftIdentity,
      NekoLaw.monoidKRightIdentity -> monoidKRightIdentity
    )

  def all[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.monoidKAll, SemigroupKProps[F].all[A], props[A])
}

object MonoidKProps {
  def apply[F[_]: MonoidK]: MonoidKProps[F] = new MonoidKProps[F] {
    val laws: MonoidKLaws[F] = MonoidKLaws[F]
  }
}
