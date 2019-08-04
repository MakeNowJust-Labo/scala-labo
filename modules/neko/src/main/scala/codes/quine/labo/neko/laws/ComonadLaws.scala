package codes.quine.labo
package neko
package laws

import syntax._

trait ComonadLaws[F[_]] {
  implicit val F: Comonad[F]

  def comonadExtractCoflattenIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> fa.coflatten.extract

  def comonadMapCoflattenIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> fa.coflatten.map(_.extract)

  def comonadMapCoflatMapCoherence[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.map(f) <-> fa.coflatMap(fa => f(fa.extract))

  def comonadLeftIdentity[A](fa: F[A]): IsEq[F[A]] =
    fa <-> fa.coflatMap(_.extract)

  def comonadRightIdneity[A, B](fa: F[A], f: F[A] => B): IsEq[B] =
    fa.coflatMap(f).extract <-> f(fa)
}

object ComonadLaws {
  def apply[F[_]: Comonad]: ComonadLaws[F] = new ComonadLaws[F] {
    val F: Comonad[F] = Comonad[F]
  }
}
