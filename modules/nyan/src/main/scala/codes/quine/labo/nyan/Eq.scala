package codes.quine.labo.nyan

import simulacrum.typeclass

@typeclass trait Eq[A] { self =>
  @simulacrum.op("===", alias = true)
  def eqv(x: A, y: A): Boolean

  @simulacrum.op("=!=", alias = true)
  def neqv(x: A, y: A): Boolean = !eqv(x, y)

  @simulacrum.noop
  final def by[B](f: B => A): Eq[B] = Eq.eqv((x, y) => self.eqv(f(x), f(y)))

  @simulacrum.noop
  final def and(other: Eq[A]): Eq[A] = Eq.eqv((x, y) => self.eqv(x, y) && other.eqv(x, y))

  @simulacrum.noop
  final def or(other: Eq[A]): Eq[A] = Eq.eqv((x, y) => self.eqv(x, y) || other.eqv(x, y))
}

object Eq {
  def eqv[A](f: (A, A) => Boolean): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = f(x, y)
  }

  def default[A]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = x == y

    override def neqv(x: A, y: A): Boolean = x != y
  }
}
