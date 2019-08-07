package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Strong[F[_, _]] extends Profunctor[F] {
  def first[A, B, C](f: F[A, B]): F[(A, C), (B, C)]
  def second[A, B, C](f: F[A, B]): F[(C, A), (C, B)] =
    dimap(first[A, B, C](f))((_: (C, A)).swap)(_.swap)
}
