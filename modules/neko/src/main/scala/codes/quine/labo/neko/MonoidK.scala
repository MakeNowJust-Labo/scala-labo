package codes.quine.labo.neko

import simulacrum.typeclass

@typeclass trait MonoidK[F[_]] {
  def empty[A]: F[A]

  @simulacrum.op("<+>", alias = true)
  def combine[A](x: F[A], y: F[A]): F[A]
}
