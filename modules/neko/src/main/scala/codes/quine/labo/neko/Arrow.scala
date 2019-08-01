package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Arrow[F[_, _]] extends Category[F] {
  def lift[A, B](f: A => B): F[A, B]

  def first[A, B, C](f: F[A, B]): F[(A, C), (B, C)]

  def second[A, B, C](f: F[A, B]): F[(C, A), (C, B)] =
    andThen(andThen(lift[(C, A), (A, C)](_.swap))(first[A, B, C](f)))(lift[(B, C), (C, B)](_.swap))

  @simulacrum.op("***", alias = true)
  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first[A, B, C](f))(second(g))

  @simulacrum.op("&&&", alias = true)
  def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] =
    andThen(lift((x: A) => (x, x)))(split(f, g))
}
