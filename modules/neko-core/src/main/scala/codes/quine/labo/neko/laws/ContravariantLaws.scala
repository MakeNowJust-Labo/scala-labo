package codes.quine.labo
package neko
package laws

import instances._, syntax._

trait ContravariantLaws[F[_]] {
  implicit val F: Contravariant[F]

  def contravariantIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> fa.contramap(a => a)

  def contravariantComposition[A, B, C](fa: F[A], f: B => A, g: C => B): IsEq[F[C]] =
    fa.contramap(f).contramap(g) <-> fa.contramap(f <<< g)
}

object ContravariantLaws {
  def apply[F[_]: Contravariant]: ContravariantLaws[F] = new ContravariantLaws[F] {
    val F: Contravariant[F] = Contravariant[F]
  }
}
