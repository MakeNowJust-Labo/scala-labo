package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait Monad[F[_]] extends Applicative[F] with FlatMap[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)(f => flatMap(fa)(a => pure(f(a))))
}
