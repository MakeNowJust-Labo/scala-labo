package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Comonad[F[_]] extends Functor[F] {
  def extract[A](fa: F[A]): A

  def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B]

  def coflatten[A](fa: F[A]): F[F[A]] = coflatMap(fa)(fa => fa)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = coflatMap(fa)(fa => f(extract(fa)))
}
