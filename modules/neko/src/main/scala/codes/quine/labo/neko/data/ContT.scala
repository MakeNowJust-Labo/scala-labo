package codes.quine.labo.neko
package data

final case class ContT[R, M[_], A](run: (A => M[R]) => M[R])

object ContT {
  implicit def ContTInstances[R, M[_]: Defer]: Monad[ContT[R, M, ?]] = new Monad[ContT[R, M, ?]] {
    def pure[A](a: A): ContT[R, M, A] = ContT(k => k(a))

    def flatMap[A, B](fa: ContT[R, M, A])(f: A => ContT[R, M, B]): ContT[R, M, B] =
      ContT(k => fa.run(a => f(a).run(k)))

    override def tailRecM[A, B](a: A)(f: A => ContT[R, M, Either[A, B]]): ContT[R, M, B] =
      ContT { k =>
        f(a).run {
          case Left(a)  => Defer[M].defer(tailRecM(a)(f).run(k))
          case Right(b) => k(b)
        }
      }
  }
}
