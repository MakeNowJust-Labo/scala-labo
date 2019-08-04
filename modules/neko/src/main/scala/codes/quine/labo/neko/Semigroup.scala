package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Semigroup[A] { self =>
  @simulacrum.op("|+|", alias = true)
  def concat(x: A, y: A): A

  @simulacrum.noop
  def by[B](to: A => B)(from: B => A): Semigroup[B] = new Semigroup[B] {
    def concat(x: B, y: B): B = to(self.concat(from(x), from(y)))
  }
}
