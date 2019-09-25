package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def widen[A, AA >: A](fa: F[A]): F[AA] = map(fa)(identity[AA])
}
