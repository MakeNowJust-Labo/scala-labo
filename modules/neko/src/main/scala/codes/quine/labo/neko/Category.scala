package codes.quine.labo.neko

import simulacrum.typeclass

@typeclass trait Category[=>:[_, _]] {
  def id[A]: A =>: A

  @simulacrum.op("<<<", alias = true)
  def compose[A, B, C](g: B =>: C)(f: A =>: B): A =>: C

  @simulacrum.op(">>>", alias = true)
  def andThen[A, B, C](f: A =>: B)(g: B =>: C): A =>: C = compose(g)(f)
}
