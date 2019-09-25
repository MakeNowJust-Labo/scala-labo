package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait MonoidK[F[_]] extends SemigroupK[F] {
  def emptyK[A]: F[A]

  @simulacrum.noop
  override def algebra[A]: Monoid[F[A]] = new Monoid[F[A]] {
    def empty: F[A] = emptyK
    def concat(x: F[A], y: F[A]): F[A] = concatK(x, y)
  }
}
