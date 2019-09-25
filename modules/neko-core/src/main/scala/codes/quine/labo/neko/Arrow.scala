package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Arrow[F[_, _]] extends Category[F] with Strong[F] {
  def lift[A, B](f: A => B): F[A, B]

  override def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D] =
    andThen(lift(f), andThen(fab, lift(g)))

  @simulacrum.op("***", alias = true)
  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] =
    andThen(first[A, B, C](f), second(g))

  @simulacrum.op("&&&", alias = true)
  def merge[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] =
    andThen(lift((x: A) => (x, x)), split(f, g))
}
