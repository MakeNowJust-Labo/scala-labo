package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait SemigroupK[F[_]] {
  @simulacrum.op("<+>", alias = true)
  def concatK[A](x: F[A], y: F[A]): F[A]

  @simulacrum.noop
  def algebra[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    def concat(x: F[A], y: F[A]): F[A] = concatK(x, y)
  }
}
