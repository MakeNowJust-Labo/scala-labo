package codes.quine.labo.nyan

import simulacrum.typeclass

@typeclass trait Arrow[=>:[_, _]] extends Category[=>:] {
  def lift[A, B](f: A => B): A =>: B

  def first[A, B, C](f: A =>: B): (A, C) =>: (B, C)

  def second[A, B, C](f: A =>: B): (C, A) =>: (C, B) =
    andThen(andThen(lift[(C, A), (A, C)](_.swap))(first[A, B, C](f)))(lift[(B, C), (C, B)](_.swap))

  @simulacrum.op("***", alias = true)
  def split[A, B, C, D](f: A =>: B, g: C =>: D): (A, C) =>: (B, D) =
    andThen(first[A, B, C](f))(second(g))

  @simulacrum.op("&&&", alias = true)
  def merge[A, B, C](f: A =>: B, g: A =>: C): A =>: (B, C) =
    andThen(lift((x: A) => (x, x)))(split(f, g))
}
