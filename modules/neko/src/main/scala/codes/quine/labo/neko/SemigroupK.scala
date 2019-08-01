package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait SemigroupK[F[_]] {
  @simulacrum.op("<+>", alias = true)
  def concatK[A](x: F[A], y: F[A]): F[A]
}
