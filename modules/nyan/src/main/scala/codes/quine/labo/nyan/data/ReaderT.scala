package codes.quine.labo.nyan
package data

final case class ReaderT[E, M[_], A](run: E => M[A])

object ReaderT {
  def read[E, M[_]](implicit M: Monad[M]): ReaderT[E, M, E] = ReaderT(e => M.pure(e))

  implicit def ReaderTInstances[E, M[_]](implicit M: Monad[M]): Monad[ReaderT[E, M, ?]] =
    new Monad[ReaderT[E, M, ?]] {
      def pure[A](a: A): ReaderT[E, M, A] = ReaderT(_ => M.pure(a))

      def flatMap[A, B](fa: ReaderT[E, M, A])(f: A => ReaderT[E, M, B]): ReaderT[E, M, B] =
        ReaderT(e => M.flatMap(fa.run(e))(f(_).run(e)))

      override def tailRecM[A, B](a: A)(f: A => ReaderT[E, M, Either[A, B]]): ReaderT[E, M, B] =
        ReaderT(e => M.tailRecM(a)(f(_).run(e)))
    }
}
