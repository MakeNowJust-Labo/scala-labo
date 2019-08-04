package codes.quine.labo
package neko
package laws

import syntax._

trait FlatMapLaws[F[_]] {
  implicit val F: FlatMap[F]

  def flatMapAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): IsEq[F[C]] =
    fa.flatMap(f).flatMap(g) <-> fa.flatMap(a => f(a).flatMap(g))
}

object FlatMapLaws {
  def apply[F[_]](implicit instance: FlatMap[F]): FlatMapLaws[F] = new FlatMapLaws[F] {
    val F: FlatMap[F] = instance
  }
}
