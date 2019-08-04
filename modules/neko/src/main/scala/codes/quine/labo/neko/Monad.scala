package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Monad[F[_]] extends Applicative[F] with FlatMap[F] {
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = flatMap(f(a)) {
    case Left(a1) => tailRecM(a1)(f)
    case Right(b) => pure(b)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)(f => flatMap(fa)(a => pure(f(a))))
}
