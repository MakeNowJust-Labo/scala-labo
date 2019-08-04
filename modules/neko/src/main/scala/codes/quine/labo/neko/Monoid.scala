package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] { self =>
  def empty: A

  @simulacrum.noop
  override def by[B](to: A => B)(from: B => A): Monoid[B] = new Monoid[B] {
    def empty: B = to(self.empty)
    def concat(x: B, y: B): B = to(self.concat(from(x), from(y)))
  }
}
