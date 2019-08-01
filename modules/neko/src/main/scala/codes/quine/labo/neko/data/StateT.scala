package codes.quine.labo
package neko
package data

final case class StateT[S, M[_], A](run: S => M[(S, A)])

object StateT {
  def read[S, M[_]](implicit M: Monad[M]): StateT[S, M, S] = StateT(s => M.pure((s, s)))

  def write[S, M[_]](s: S)(implicit M: Monad[M]): StateT[S, M, Unit] = StateT(_ => M.pure((s, ())))

  implicit def StateTInstances[S, M[_]](implicit M: Monad[M]): Monad[StateT[S, M, ?]] =
    new Monad[StateT[S, M, ?]] {
      def pure[A](a: A): StateT[S, M, A] = StateT(s => M.pure((s, a)))

      def flatMap[A, B](fa: StateT[S, M, A])(f: A => StateT[S, M, B]): StateT[S, M, B] =
        StateT(s1 => M.flatMap(fa.run(s1)) { case (s2, a) => f(a).run(s2) })

      override def tailRecM[A, B](a: A)(f: A => StateT[S, M, Either[A, B]]): StateT[S, M, B] =
        StateT(
          s =>
            M.tailRecM[(S, A), (S, B)]((s, a)) {
              case (s1, a1) =>
                M.map(f(a1).run(s1)) {
                  case (s2, Left(a2)) => Left((s2, a2))
                  case (s2, Right(b)) => Right((s2, b))
                }
            }
        )
    }
}
