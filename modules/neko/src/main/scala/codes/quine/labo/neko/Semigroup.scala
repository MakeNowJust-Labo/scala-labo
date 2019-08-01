package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Semigroup[A] {
  @simulacrum.op("<>", alias = true)
  def concat(x: A, y: A): A
}
