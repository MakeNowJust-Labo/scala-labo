package codes.quine.labo.nyan
package laws

import syntax._

trait MonoidKLaws[F[_]] {
  implicit val F: MonoidK[F]

  def monoidKLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> (F.empty[A] <+> fa)

  def monoidKRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> (fa <+> F.empty[A])

  def monoidKAssociativity[A](x: F[A], y: F[A], z: F[A]): IsEq[F[A]] =
    ((x <+> y) <+> z) <-> (x <+> (y <+> z))
}

object MonoidKLaws {
  def apply[F[_]](implicit instance: MonoidK[F]): MonoidKLaws[F] = new MonoidKLaws[F] {
    implicit override val F: MonoidK[F] = instance
  }
}
