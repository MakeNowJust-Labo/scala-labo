package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait MonoidK[F[_]] extends SemigroupK[F] {
  def emptyK[A]: F[A]
}
