package codes.quine.labo
package neko

import scala.math

import simulacrum.typeclass

@typeclass trait Eq[A] { self =>
  @simulacrum.op("===", alias = true)
  def eqv(x: A, y: A): Boolean

  @simulacrum.op("=!=", alias = true)
  def neqv(x: A, y: A): Boolean = !eqv(x, y)

  @simulacrum.noop
  def by[B](f: B => A): Eq[B] = Eq.eqv((x, y) => self.eqv(f(x), f(y)))
}

object Eq {
  def eqv[A](f: (A, A) => Boolean): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = f(x, y)
  }

  def fromEquiv[A: math.Equiv]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = math.Equiv[A].equiv(x, y)
  }
}
