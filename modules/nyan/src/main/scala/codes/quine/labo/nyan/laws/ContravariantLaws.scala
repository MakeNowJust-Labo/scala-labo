package codes.quine.labo.nyan
package laws

import syntax._

trait ContravariantLaws[F[_]] {
  implicit val F: Contravariant[F]

  def contravariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> fa.contramap(a => a)

  def contravariantComposition[A, B, C](fa: F[A], f: B => A, g: C => B): IsEq[F[C]] =
    fa.contramap(f).contramap(g) <-> fa.contramap(f compose g)
}

object ContravariantLaws {
  def apply[F[_]](implicit instance: Contravariant[F]): ContravariantLaws[F] = new ContravariantLaws[F] {
    override implicit val F: Contravariant[F] = instance
  }
}