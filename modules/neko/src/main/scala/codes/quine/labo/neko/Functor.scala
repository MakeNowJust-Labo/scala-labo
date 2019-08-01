package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
