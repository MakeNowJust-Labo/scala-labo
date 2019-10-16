package codes.quine.labo
package neko

import simulacrum.typeclass
import data._, instances.list._

@typeclass trait Traverse[F[_]] extends Functor[F] with Fold[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  override def foldMap[A, M: Monoid](fa: F[A])(f: A => M): M =
    traverse(fa)(a => Const[M, M](f(a))).value

  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B =
    foldMap(fa)(a => List(a)).foldLeft(b)(f)

  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    foldMap(fa)(a => Endo.lift[Eval[B]](lb => Eval.defer(f(a, lb))))(MonoidK[Endo].algebra).apply(lb)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(a => Id(f(a))).value
}
