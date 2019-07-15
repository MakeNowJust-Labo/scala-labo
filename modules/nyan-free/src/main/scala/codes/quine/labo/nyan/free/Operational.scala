package codes.quine.labo.nyan
package free

import data._, syntax._

case class Operational[F[_], A](free: Free[Coyoneda[F, ?], A]) {
  def interpret[M[_]](t: F ~> M)(implicit M: Monad[M]): M[A] =
    M.tailRecM(free) {
      case Pure(a) => M.pure(Right(a))
      case Flatten(ffa) => ffa.mapK(t).run.map(Left(_))
    }
}

object Operational {
  def lift[F[_], A](fa: F[A]) = Operational(Free.lift(Coyoneda.lift(fa)))

  implicit def OperationalMonad[F[_]]: Monad[Operational[F, ?]] = new Monad[Operational[F, ?]] {
    def pure[A](a: A): Operational[F, A] = Operational(Pure(a))

    def flatMap[A, B](fa: Operational[F, A])(f: A => Operational[F, B]): Operational[F, B] =
      Operational(fa.free.flatMap(a => f(a).free))
  }
}