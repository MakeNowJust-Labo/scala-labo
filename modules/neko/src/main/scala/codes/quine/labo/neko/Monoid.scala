package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] {
  def empty: A
}
