package codes.quine.labo.neko

import simulacrum.typeclass

@typeclass trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  @simulacrum.op("<*>", alias = true)
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(pure(f))(fa)
}
