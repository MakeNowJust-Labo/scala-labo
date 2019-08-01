package codes.quine.labo
package neko
package laws

import syntax._

trait SemigroupKLaws[F[_]] {
  implicit val F: SemigroupK[F]

  def semigroupKAssociativity[A](x: F[A], y: F[A], z: F[A]): IsEq[F[A]] =
    ((x <+> y) <+> z) <-> (x <+> (y <+> z))
}

object SemigroupKLaws {
  def apply[F[_]](implicit instance: SemigroupK[F]): SemigroupKLaws[F] = new SemigroupKLaws[F] {
    val F: SemigroupK[F] = instance
  }
}
