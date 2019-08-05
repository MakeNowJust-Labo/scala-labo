package codes.quine.labo
package neko
package laws

import syntax._

trait CoflatMapLaws[F[_]] {
  implicit val F: CoflatMap[F]

  def coflatMapAssociativity[A, B, C](fa: F[A], f: F[A] => B, g: F[B] => C): IsEq[F[C]] =
    fa.coflatMap(f).coflatMap(g) <-> fa.coflatMap(fa => g(fa.coflatMap(f)))

  def coflatMapCoflattenThroughMap[A](fa: F[A]): IsEq[F[F[F[A]]]] =
    fa.coflatten.coflatten <-> fa.coflatten.map(_.coflatten)

  def coflatMapCoherence[A, B](fa: F[A], f: F[A] => B): IsEq[F[B]] =
    fa.coflatMap(f) <-> fa.coflatten.map(f)

  def coflatMapIdentity[A](fa: F[A]): IsEq[F[F[A]]] =
    fa.coflatten <-> fa.coflatMap(identity)
}

object CoflatMapLaws {
  def apply[F[_]: CoflatMap]: CoflatMapLaws[F] = new CoflatMapLaws[F] {
    val F: CoflatMap[F] = CoflatMap[F]
  }
}
