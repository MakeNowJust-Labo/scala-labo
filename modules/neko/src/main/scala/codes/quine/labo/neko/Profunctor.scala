package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Profunctor[F[_, _]] {
  def dimap[A, B, C, D](fa: F[A, B])(f: C => A)(g: B => D): F[C, D]

  def lmap[A, B, C](fa: F[A, B])(f: C => A): F[C, B] = dimap[A, B, C, B](fa)(f)(identity)
  def rmap[A, B, C](fa: F[A, B])(f: B => C): F[A, C] = dimap[A, B, A, C](fa)(identity)(f)
}
