package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait CoflatMap[F[_]] extends Functor[F] {
  def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B]

  def coflatten[A](fa: F[A]): F[F[A]] = coflatMap(fa)(fa => fa)
}
