package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] { self =>
  def empty: A

  @simulacrum.noop
  override def by[B](embed: B => A, eject: A => B): Monoid[B] = new Monoid[B] {
    def empty: B = eject(self.empty)
    def concat(x: B, y: B): B = eject(self.concat(embed(x), embed(y)))
  }
}
