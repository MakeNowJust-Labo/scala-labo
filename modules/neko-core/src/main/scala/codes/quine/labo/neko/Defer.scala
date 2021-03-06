package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Defer[F[_]] {
  def defer[A](fa: => F[A]): F[A]
}
