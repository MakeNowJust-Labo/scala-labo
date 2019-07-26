package codes.quine.labo.neko
package data

import syntax._

final case class ReaderT[E, M[_], A](run: E => M[A]) {
  def map[B](f: A => B)(implicit M: Functor[M]): ReaderT[E, M, B] = ReaderT(e => run(e).map(f))

  def ap[B](ff: ReaderT[E, M, A => B])(implicit M: Applicative[M]): ReaderT[E, M, B] =
    ReaderT(e => ff.run(e) <*> run(e))

  def flatMap[B](f: A => ReaderT[E, M, B])(implicit M: Monad[M]): ReaderT[E, M, B] =
    ReaderT(e => run(e).flatMap(f(_).run(e)))

  def compose[D](g: ReaderT[D, M, E])(implicit M: Monad[M]): ReaderT[D, M, A] = ReaderT(d => g.run(d).flatMap(run))

  def andThen[B](g: ReaderT[A, M, B])(implicit M: Monad[M]): ReaderT[E, M, B] = ReaderT(e => run(e).flatMap(g.run))
}

object ReaderT {
  def ask[E, M[_]](implicit M: Monad[M]): ReaderT[E, M, E] = ReaderT(e => M.pure(e))

  private[this] trait ReaderTFunctorInstance[E, M[_]] extends Functor[ReaderT[E, M, ?]] {
    implicit def M: Functor[M]

    def map[A, B](fa: ReaderT[E, M, A])(f: A => B): ReaderT[E, M, B] = fa.map(f)
  }

  private[this] trait ReaderTApplicativeInstance[E, M[_]]
      extends ReaderTFunctorInstance[E, M]
      with Applicative[ReaderT[E, M, ?]] {
    implicit def M: Applicative[M]

    def pure[A](a: A): ReaderT[E, M, A] = ReaderT(e => M.pure(a))

    def ap[A, B](ff: ReaderT[E, M, A => B])(fa: ReaderT[E, M, A]): ReaderT[E, M, B] = fa.ap(ff)
  }

  private[this] trait ReaderTMonadInstance[E, M[_]]
      extends ReaderTApplicativeInstance[E, M]
      with Monad[ReaderT[E, M, ?]] {
    implicit def M: Monad[M]

    def flatMap[A, B](fa: ReaderT[E, M, A])(f: A => ReaderT[E, M, B]): ReaderT[E, M, B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => ReaderT[E, M, Either[A, B]]): ReaderT[E, M, B] =
      ReaderT(e => M.tailRecM(a)(f(_).run(e)))
  }

  implicit def ReaderTFunctorInstances[E, M[_]](implicit instance: Functor[M]): Functor[ReaderT[E, M, ?]] =
    new ReaderTFunctorInstance[E, M] {
      implicit def M: Functor[M] = instance
    }

  implicit def ReaderTApplicativeInstances[E, M[_]](implicit instance: Applicative[M]): Applicative[ReaderT[E, M, ?]] =
    new ReaderTApplicativeInstance[E, M] {
      implicit def M: Applicative[M] = instance
    }

  implicit def ReaderTMonadInstances[E, M[_]](implicit instance: Monad[M]): Monad[ReaderT[E, M, ?]] =
    new ReaderTMonadInstance[E, M] {
      implicit def M: Monad[M] = instance
    }
}
