package codes.quine.labo
package neko
package laws

import syntax._

trait FlatMapLaws[F[_]] {
  implicit val F: FlatMap[F]

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) <-> fa.flatMap(a => f(a).flatMap(g))

  def flatMapTailRecMConsistency[A, B](fa: F[A], f: A => F[B]): IsEq[F[B]] = {
    def defaultFlatMap[A1, B1](fa: F[A1])(f: A1 => F[B1]): F[B1] =
      F.tailRecM[Option[A1], B1](None) {
        case None => fa.map(a => Left(Some(a)))
        case Some(a) => f(a).map(Right(_))
      }

    F.flatMap(fa)(f) <-> defaultFlatMap(fa)(f)
  }
}

object FlatMapLaws {
  def apply[F[_]](implicit instance: FlatMap[F]): FlatMapLaws[F] = new FlatMapLaws[F] {
    val F: FlatMap[F] = instance
  }
}
