package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Comonad[F[_]] extends CoflatMap[F] {
  def extract[A](fa: F[A]): A

  override def map[A, B](fa: F[A])(f: A => B): F[B] = coflatMap(fa)(fa => f(extract(fa)))
}
