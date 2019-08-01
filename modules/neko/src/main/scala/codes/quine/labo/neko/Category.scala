package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Category[F[_, _]] {
  def id[A]: F[A, A]

  @simulacrum.op("<<<", alias = true)
  def compose[A, B, C](g: F[B, C])(f: F[A, B]): F[A, C]

  @simulacrum.op(">>>", alias = true)
  def andThen[A, B, C](f: F[A, B])(g: F[B, C]): F[A, C] = compose(g)(f)
}
