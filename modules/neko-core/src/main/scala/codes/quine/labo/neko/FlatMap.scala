package codes.quine.labo
package neko

import simulacrum.typeclass

@typeclass trait FlatMap[F[_]] extends Apply[F] {
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    tailRecM[Option[A], B](None) {
      case None    => map(fa)(a => Left(Some(a)))
      case Some(a) => map(f(a))(Right(_))
    }

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = flatMap(ff)(f => map(fa)(f))
}
