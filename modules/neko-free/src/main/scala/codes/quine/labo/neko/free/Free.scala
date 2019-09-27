package codes.quine.labo
package neko
package free

import data._, syntax._

sealed trait Free[F[_], A]

case class Pure[F[_], A](a: A) extends Free[F, A]
case class Flatten[F[_], A](ffa: F[Free[F, A]]) extends Free[F, A]

object Free {

  def lift[F[_], A](fa: F[A])(implicit F: Functor[F]): Free[F, A] = Flatten(F.map(fa)(Pure(_)))

  def foldFree[F[_], A, M[_]](fa: Free[F, A])(t: F ~> M)(implicit M: Monad[M]): M[A] =
    M.tailRecM(fa) {
      case Pure(a)      => M.pure(Right(a))
      case Flatten(ffa) => t(ffa).map(Left(_))
    }

  def iterate[F[_], A](fa: Free[F, A])(f: F[A] => A)(implicit F: Functor[F]): A =
    fa match {
      case Pure(a)      => a
      case Flatten(ffa) => f(F.map(ffa)((fa: Free[F, A]) => iterate(fa)(f)))
    }

  implicit def freeMonadInstance[F[_]](implicit F: Functor[F]): Monad[Free[F, *]] = new Monad[Free[F, *]] {
    def pure[A](a: A): Free[F, A] = Pure(a)

    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa match {
        case Pure(a)      => f(a)
        case Flatten(ffa) => Flatten(F.map(ffa)((fa: Free[F, A]) => flatMap(fa)(f)))
      }

    def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
      f(a).flatMap {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
  }
}
