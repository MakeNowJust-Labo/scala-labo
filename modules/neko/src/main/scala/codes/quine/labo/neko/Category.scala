package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Category[F[_, _]] extends Compose[F] {
  def identity[A]: F[A, A]
}
