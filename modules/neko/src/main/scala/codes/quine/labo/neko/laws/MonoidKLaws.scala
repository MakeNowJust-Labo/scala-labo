package codes.quine.labo
package neko
package laws

import syntax._

trait MonoidKLaws[F[_]] {
  implicit val F: MonoidK[F]

  def monoidKLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> (F.emptyK[A] <+> fa)

  def monoidKRightIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> (fa <+> F.emptyK[A])
}

object MonoidKLaws {
  def apply[F[_]](implicit instance: MonoidK[F]): MonoidKLaws[F] = new MonoidKLaws[F] {
    val F: MonoidK[F] = instance
  }
}
