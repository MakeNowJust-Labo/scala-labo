package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Compose[F[_, _]] {
  @simulacrum.op("<<<", alias = true)
  def compose[A, B, C](f: F[B, C], g: F[A, B]): F[A, C]

  @simulacrum.op(">>>", alias = true)
  def andThen[A, B, C](f: F[A, B], g: F[B, C]): F[A, C] = compose(g, f)
}
