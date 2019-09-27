package codes.quine.labo
package neko

import simulacrum.typeclass
import data._

@typeclass trait Traverse[F[_]] extends Functor[F] with Fold[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(a => Id(f(a))).value
}
