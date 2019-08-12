package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]

  def narrow[A, AA <: A](fa: F[A]): F[AA] = contramap(fa)(identity[AA])
}
