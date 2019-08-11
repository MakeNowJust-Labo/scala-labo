package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Semigroup[A] { self =>
  @simulacrum.op("|+|", alias = true)
  def concat(x: A, y: A): A

  @simulacrum.noop
  def by[B](embed: B => A, eject: A => B): Semigroup[B] = new Semigroup[B] {
    def concat(x: B, y: B): B = eject(self.concat(embed(x), embed(y)))
  }
}
