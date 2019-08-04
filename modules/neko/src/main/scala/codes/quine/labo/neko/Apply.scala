package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Apply[F[_]] extends Functor[F] {
  @simulacrum.op("<*>", alias = true)
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}
