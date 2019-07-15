package codes.quine.labo.nyan

import simulacrum.typeclass

@typeclass trait Eq[A] {
  @simulacrum.op("===", alias = true)
  def eqv(x: A, y: A): Boolean

  @simulacrum.op("=!=", alias = true)
  def neqv(x: A, y: A): Boolean = !eqv(x, y)
}
