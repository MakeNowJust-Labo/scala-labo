package codes.quine.labo
package neko

import data._

import simulacrum.typeclass

@typeclass trait Fold[F[_]] {
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B

  def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def fold[A: Monoid](fa: F[A]): A =
    foldLeft(fa, Monoid[A].empty)(Monoid[A].concat(_, _))

  def foldMap[A, M: Monoid](fa: F[A])(f: A => M): M =
    foldLeft(fa, Monoid[M].empty)((a, b) => Monoid[M].concat(a, f(b)))

  def isEmpty[A](fa: F[A]): Boolean =
    foldRight(fa, Eval.now(true))((_, _) => Eval.now(false)).value

  def size[A](fa: F[A]): Int = foldLeft(fa, 0)((i, _) => i + 1)
}
