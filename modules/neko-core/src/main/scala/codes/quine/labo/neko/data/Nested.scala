package codes.quine.labo
package neko
package data

import syntax._
import instances.string._

case class Nested[F[_], G[_], A](value: F[G[A]])

object Nested extends NestedInstances0

trait NestedInstances0 extends NestedInstances1 {
  implicit def nestedDeferInstance[F[_]: Defer, G[_]]: Defer[Nested[F, G, *]] =
    new Defer[Nested[F, G, *]] {
      def defer[A](fa: => Nested[F, G, A]): Nested[F, G, A] = Nested(Defer[F].defer(fa.value))
    }

  implicit def nestedFunctorInstance[F[_]: Functor, G[_]: Functor]: Functor[Nested[F, G, *]] =
    new Functor[Nested[F, G, *]] {
      def map[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(fa.value.map(_.map(f)))
    }

  implicit def nestedApplyInstance[F[_]: Apply, G[_]: Apply]: Apply[Nested[F, G, *]] =
    new Apply[Nested[F, G, *]] {
      def map[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(fa.value.map(_.map(f)))

      def ap[A, B](ff: Nested[F, G, A => B])(fa: Nested[F, G, A]): Nested[F, G, B] =
        Nested(Apply[F].map2(ff.value, fa.value)(_ <*> _))
    }

  implicit def nestedApplicativeInstance[F[_]: Applicative, G[_]: Applicative]: Applicative[Nested[F, G, *]] =
    new Applicative[Nested[F, G, *]] {
      override def map[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] =
        Nested(fa.value.map(_.map(f)))

      def ap[A, B](ff: Nested[F, G, A => B])(fa: Nested[F, G, A]): Nested[F, G, B] =
        Nested(Apply[F].map2(ff.value, fa.value)(_ <*> _))

      def pure[A](a: A): Nested[F, G, A] = Nested(Applicative[F].pure(Applicative[G].pure(a)))
    }

  implicit def nestedFoldInstance[F[_]: Fold, G[_]: Fold]: Fold[Nested[F, G, *]] =
    new Fold[Nested[F, G, *]] {
      def foldLeft[A, B](fa: Nested[F, G, A], b: B)(f: (B, A) => B): B =
        fa.value.foldLeft(b)((b, ga) => ga.foldLeft(b)(f))

      def foldRight[A, B](fa: Nested[F, G, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.value.foldRight(lb)((ga, lb) => ga.foldRight(lb)(f))
    }

  implicit def nestedEqInstance[F[_], G[_], A](implicit FGA: Eq[F[G[A]]]): Eq[Nested[F, G, A]] = FGA.by(_.value)
  implicit def nestedPartialOrdInstance[F[_], G[_], A](implicit FGA: PartialOrd[F[G[A]]]): PartialOrd[Nested[F, G, A]] =
    FGA.by(_.value)
  implicit def nestedOrdInstance[F[_], G[_], A](implicit FGA: Ord[F[G[A]]]): Ord[Nested[F, G, A]] = FGA.by(_.value)
}

trait NestedInstances1 {
  implicit def nestedHashInstance[F[_], G[_], A](implicit FGA: Hash[F[G[A]]]): Hash[Nested[F, G, A]] =
    new Hash[Nested[F, G, A]] {
      def eqv(x: Nested[F, G, A], y: Nested[F, G, A]): Boolean = x.value === y.value
      def hash(x: Nested[F, G, A]): Int = "Nested".hash * 31 + x.value.hash
    }
}
