package codes.quine.labo.neko

import simulacrum.typeclass

@typeclass trait Defer[F[_]] {
  def defer[A](fa: => F[A]): F[A]
}
