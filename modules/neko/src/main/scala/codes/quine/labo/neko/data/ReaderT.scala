package codes.quine.labo
package neko
package data

import syntax._

final case class ReaderT[F[_], E, A](run: E => F[A]) {
  def map[B](f: A => B)(implicit F: Functor[F]): ReaderT[F, E, B] = ReaderT(e => run(e).map(f))

  def flatMap[B](f: A => ReaderT[F, E, B])(implicit F: FlatMap[F]): ReaderT[F, E, B] =
    ReaderT(e => run(e).flatMap(f(_).run(e)))
}

object ReaderT extends ReaderTInstances0 {
  def run[F[_], E, A](fa: ReaderT[F, E, A])(e: E): F[A] = fa.run(e)

  def ask[F[_]: Applicative, E]: ReaderT[F, E, E] = ReaderT(e => Applicative[F].pure(e))
  def local[F[_], E, A](f: E => E)(fa: ReaderT[F, E, A]): ReaderT[F, E, A] =
    ReaderT(e => fa.run(f(e)))

  def tailRecM[F[_]: FlatMap, E, A, B](a: A)(f: A => ReaderT[F, E, Either[A, B]]): ReaderT[F, E, B] =
    ReaderT(e => FlatMap[F].tailRecM(a)(f(_).run(e)))
}

private[data] trait ReaderTInstances0 extends ReaderTInstances1 {
  implicit def readerTFunctorInstance[F[_]: Functor, E]: Functor[ReaderT[F, E, *]] = new Functor[ReaderT[F, E, *]] {
    def map[A, B](fa: ReaderT[F, E, A])(f: A => B): ReaderT[F, E, B] = fa.map(f)
  }

  implicit def readerTApplyInstance[F[_]: Apply, E]: Apply[ReaderT[F, E, *]] = new Apply[ReaderT[F, E, *]] {
    def map[A, B](fa: ReaderT[F, E, A])(f: A => B): ReaderT[F, E, B] = fa.map(f)
    def ap[A, B](ff: ReaderT[F, E, A => B])(fa: ReaderT[F, E, A]): ReaderT[F, E, B] = ReaderT(e => ff.run(e) <*> fa.run(e))
  }

  implicit def readerTApplicativeInstance[F[_]: Applicative, E]: Applicative[ReaderT[F, E, *]] = new Applicative[ReaderT[F, E, *]] {
    def pure[A](a: A): ReaderT[F, E, A] = ReaderT(e => Applicative[F].pure(a))
    override def map[A, B](fa: ReaderT[F, E, A])(f: A => B): ReaderT[F, E, B] = fa.map(f)
    def ap[A, B](ff: ReaderT[F, E, A => B])(fa: ReaderT[F, E, A]): ReaderT[F, E, B] = ReaderT(e => ff.run(e) <*> fa.run(e))
  }

  implicit def readerTFlatMapInstance[F[_]: FlatMap, E]: FlatMap[ReaderT[F, E, *]] = new FlatMap[ReaderT[F, E, *]] {
    def map[A, B](fa: ReaderT[F, E, A])(f: A => B): ReaderT[F, E, B] = fa.map(f)
    override def flatMap[A, B](fa: ReaderT[F, E, A])(f: A => ReaderT[F, E, B]): ReaderT[F, E, B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => ReaderT[F, E, Either[A, B]]): ReaderT[F, E, B] = ReaderT.tailRecM(a)(f)
  }

  implicit def readerTMonadInstance[F[_]: Monad, E]: Monad[ReaderT[F, E, *]] = new Monad[ReaderT[F, E, *]] {
    def pure[A](a: A): ReaderT[F, E, A] = ReaderT(e => Monad[F].pure(a))
    override def flatMap[A, B](fa: ReaderT[F, E, A])(f: A => ReaderT[F, E, B]): ReaderT[F, E, B] = fa.flatMap(f)
    override def map[A, B](fa: ReaderT[F, E, A])(f: A => B): ReaderT[F, E, B] = fa.map(f)
    def tailRecM[A, B](a: A)(f: A => ReaderT[F, E, Either[A, B]]): ReaderT[F, E, B] = ReaderT.tailRecM(a)(f)
  }

  implicit def readerTSemigroupInstance[F[_], E, A](implicit FA: Semigroup[F[A]]): Semigroup[ReaderT[F, E, A]] = new Semigroup[ReaderT[F, E, A]] {
    def concat(x: ReaderT[F, E, A], y: ReaderT[F, E, A]): ReaderT[F, E, A] = ReaderT(e => x.run(e) |+| y.run(e))
  }

  implicit def readerTMonoidInstance[F[_], E, A](implicit FA: Monoid[F[A]]): Monoid[ReaderT[F, E, A]] = new Monoid[ReaderT[F, E, A]] {
    def empty: ReaderT[F, E, A] = ReaderT(e => FA.empty)
    def concat(x: ReaderT[F, E, A], y: ReaderT[F, E, A]): ReaderT[F, E, A] = ReaderT(e => x.run(e) |+| y.run(e))
  }

  implicit def readerTSemigroupKInstance[F[_]: SemigroupK, E]: SemigroupK[ReaderT[F, E, *]] = new SemigroupK[ReaderT[F, E, *]] {
    def concatK[A](x: ReaderT[F, E, A], y: ReaderT[F, E, A]): ReaderT[F, E, A] = ReaderT(e => x.run(e) <+> y.run(e))
  }

  implicit def readerTMonoidKInstance[F[_]: MonoidK, E]: MonoidK[ReaderT[F, E, *]] = new MonoidK[ReaderT[F, E, *]] {
    def emptyK[A]: ReaderT[F, E, A] = ReaderT(e => MonoidK[F].emptyK)
    def concatK[A](x: ReaderT[F, E, A], y: ReaderT[F, E, A]): ReaderT[F, E, A] = ReaderT(e => x.run(e) <+> y.run(e))
  }
}

private[data] trait ReaderTInstances1 {
  implicit def readerTAlternativeInstance[F[_]: Alternative, E]: Alternative[ReaderT[F, E, *]] = new Alternative[ReaderT[F, E, *]] {
    def pure[A](a: A): ReaderT[F, E, A] = ReaderT(e => Applicative[F].pure(a))
    override def map[A, B](fa: ReaderT[F, E, A])(f: A => B): ReaderT[F, E, B] = fa.map(f)
    def ap[A, B](ff: ReaderT[F, E, A => B])(fa: ReaderT[F, E, A]): ReaderT[F, E, B] = ReaderT(e => ff.run(e) <*> fa.run(e))
    def emptyK[A]: ReaderT[F, E, A] = ReaderT(e => Alternative[F].emptyK)
    def concatK[A](x: ReaderT[F, E, A], y: ReaderT[F, E, A]): ReaderT[F, E, A] = ReaderT(e => x.run(e) <+> y.run(e))
  }
}