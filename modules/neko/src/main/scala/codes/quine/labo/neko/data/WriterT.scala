package codes.quine.labo
package neko
package data

import instances.string._, syntax._

final case class WriterT[F[_], L, A](run: F[(L, A)]) {
  def map[B](f: A => B)(implicit F: Functor[F]): WriterT[F, L, B] =
    WriterT(run.map { case (l, a) => (l, f(a)) })

  def flatMap[B](f: A => WriterT[F, L, B])(implicit F: FlatMap[F], L: Semigroup[L]): WriterT[F, L, B] =
    WriterT(run.flatMap { case (l1, a) => f(a).run.map { case (l2, b) => (l1 |+| l2, b) } })
}

object WriterT extends WriterTInstances0 {
  def run[F[_], L, A](fa: WriterT[F, L, A]): F[(L, A)] = fa.run

  def tell[F[_]: Applicative, L](l: L): WriterT[F, L, Unit] = WriterT(Applicative[F].pure((l, ())))
  def listen[F[_]: Functor, L, A](fa: WriterT[F, L, A]): WriterT[F, L, (L, A)] =
    WriterT(fa.run.map { case (l, a) => (l, (l, a)) })

  private[data] def tailRecM[F[_]: FlatMap, L: Monoid, A, B](
    a: A
  )(f: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] =
    WriterT(FlatMap[F].tailRecM[(L, A), (L, B)]((Monoid[L].empty, a)) {
      case (l1, a) =>
        f(a).run.map {
          case (l2, Left(a1)) => Left((l1 |+| l2, a1))
          case (l2, Right(b)) => Right((l1 |+| l2, b))
        }
    })
}

private[data] trait WriterTInstances0 extends WriterTInstances1 {
  implicit def writerTEqInstance[F[_], L, A](implicit FLA: Eq[F[(L, A)]]): Eq[WriterT[F, L, A]] = FLA.by(_.run)
  implicit def writerTPartialOrdInstance[F[_], L, A](
    implicit FLA: PartialOrd[F[(L, A)]]
  ): PartialOrd[WriterT[F, L, A]] = FLA.by(_.run)
  implicit def writerTOrdInstance[F[_], L, A](implicit FLA: Ord[F[(L, A)]]): Ord[WriterT[F, L, A]] = FLA.by(_.run)

  implicit def writerTSemigroupInstance[F[_], L, A](implicit FLA: Semigroup[F[(L, A)]]): Semigroup[WriterT[F, L, A]] =
    FLA.by(WriterT(_))(_.run)
  implicit def writerTMonoidInstance[F[_], L, A](implicit FLA: Monoid[F[(L, A)]]): Monoid[WriterT[F, L, A]] =
    FLA.by(WriterT(_))(_.run)

  implicit def writerTFunctorInstance[F[_]: Functor, L]: Functor[WriterT[F, L, *]] = new Functor[WriterT[F, L, *]] {
    def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] = fa.map(f)
  }

  implicit def writerTApplyInstance[F[_]: Apply, L: Semigroup]: Apply[WriterT[F, L, *]] = new Apply[WriterT[F, L, *]] {
    def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] = fa.map(f)
    def ap[A, B](ff: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
      WriterT(Apply[F].map2(ff.run, fa.run) { case ((l1, f), (l2, a)) => (l1 |+| l2, f(a)) })
  }

  implicit def writerTApplicativeInstance[F[_]: Applicative, L: Monoid]: Applicative[WriterT[F, L, *]] =
    new Applicative[WriterT[F, L, *]] {
      def pure[A](a: A): WriterT[F, L, A] = WriterT(Applicative[F].pure((Monoid[L].empty, a)))
      override def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] = fa.map(f)
      def ap[A, B](ff: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
        WriterT(Applicative[F].map2(ff.run, fa.run) { case ((l1, f), (l2, a)) => (l1 |+| l2, f(a)) })
    }

  implicit def writerTFlatMapInstance[F[_]: FlatMap, L: Monoid]: FlatMap[WriterT[F, L, *]] =
    new FlatMap[WriterT[F, L, *]] {
      override def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] = fa.map(f)
      override def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] = fa.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = WriterT.tailRecM(a)(f)
    }

  implicit def writerTMonadInstance[F[_]: Monad, L: Monoid]: Monad[WriterT[F, L, *]] = new Monad[WriterT[F, L, *]] {
    def pure[A](a: A): WriterT[F, L, A] = WriterT(Monad[F].pure((Monoid[L].empty, a)))
    override def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] = fa.map(f)
    override def flatMap[A, B](fa: WriterT[F, L, A])(f: A => WriterT[F, L, B]): WriterT[F, L, B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => WriterT[F, L, Either[A, B]]): WriterT[F, L, B] = WriterT.tailRecM(a)(f)
  }

  implicit def writerTSemigroupKInstance[F[_]: SemigroupK, L]: SemigroupK[WriterT[F, L, *]] =
    new SemigroupK[WriterT[F, L, *]] {
      def concatK[A](x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] = WriterT(x.run <+> y.run)
    }

  implicit def writerTMonoidKInstance[F[_]: MonoidK, L]: MonoidK[WriterT[F, L, *]] = new MonoidK[WriterT[F, L, *]] {
    def emptyK[A]: WriterT[F, L, A] = WriterT(MonoidK[F].emptyK)
    def concatK[A](x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] = WriterT(x.run <+> y.run)
  }
}

private[data] trait WriterTInstances1 {
  implicit def writerTHashInstance[F[_], L, A](implicit FLA: Hash[F[(L, A)]]): Hash[WriterT[F, L, A]] =
    new Hash[WriterT[F, L, A]] {
      def eqv(x: WriterT[F, L, A], y: WriterT[F, L, A]): Boolean = x.run === y.run
      def hash(x: WriterT[F, L, A]): Int = "WriterT".hash * 31 + x.run.hash
    }

  implicit def writerTAlternativeInstance[F[_]: Alternative, L: Monoid]: Alternative[WriterT[F, L, *]] =
    new Alternative[WriterT[F, L, *]] {
      def pure[A](a: A): WriterT[F, L, A] = WriterT(Applicative[F].pure((Monoid[L].empty, a)))
      override def map[A, B](fa: WriterT[F, L, A])(f: A => B): WriterT[F, L, B] = fa.map(f)
      def ap[A, B](ff: WriterT[F, L, A => B])(fa: WriterT[F, L, A]): WriterT[F, L, B] =
        WriterT(Applicative[F].map2(ff.run, fa.run) { case ((l1, f), (l2, a)) => (l1 |+| l2, f(a)) })
      def emptyK[A]: WriterT[F, L, A] = WriterT(MonoidK[F].emptyK)
      def concatK[A](x: WriterT[F, L, A], y: WriterT[F, L, A]): WriterT[F, L, A] = WriterT(x.run <+> y.run)
    }
}
