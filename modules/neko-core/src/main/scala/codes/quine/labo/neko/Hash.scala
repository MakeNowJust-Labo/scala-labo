package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Hash[A] extends Eq[A] {
  def hash(x: A): Int
}
