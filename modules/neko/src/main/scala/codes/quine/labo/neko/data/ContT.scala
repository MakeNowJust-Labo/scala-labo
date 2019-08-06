package codes.quine.labo
package neko
package data

final case class ContT[F[_], R, A](run: (A => F[R]) => F[R]) {
  def map[B](f: A => B)(implicit F: Defer[F]): ContT[F, R, B] =
    ContT(k => F.defer(run(a => k(f(a)))))

  def flatMap[B](f: A => ContT[F, R, B])(implicit F: Defer[F]): ContT[F, R, B] =
    ContT(k => F.defer(run(a => f(a).run(k))))
}

object ContT extends ContTInstances0 {
  def now[F[_], R, A](a: A): ContT[F, R, A] = ContT(k => k(a))
  def always[F[_], R, A](a: => A): ContT[F, R, A] = ContT(k => k(a))
  def defer[F[_]: Defer, R, A](fa: => ContT[F, R, A]): ContT[F, R, A] = ContT(k => Defer[F].defer(fa.run(k)))

  def callCC[F[_], R, A](run: (A => F[R]) => F[R]): ContT[F, R, A] = ContT(run)
}

private[data] trait ContTInstances0 {
  implicit def contTMonadInstances[F[_]: Defer, R]: Monad[ContT[F, R, *]] = new Monad[ContT[F, R, *]] {
    def pure[A](a: A): ContT[F, R, A] = ContT.now(a)

    override def map[A, B](fa: ContT[F, R, A])(f: A => B): ContT[F, R, B] =
      fa.map(f)

    override def flatMap[A, B](fa: ContT[F, R, A])(f: A => ContT[F, R, B]): ContT[F, R, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => ContT[F, R, Either[A, B]]): ContT[F, R, B] =
      ContT { k =>
        def loop(a: A): F[R] =
          f(a).run {
            case Left(a1) => Defer[F].defer(loop(a1))
            case Right(b) => Defer[F].defer(k(b))
          }
        loop(a)
      }
  }

  implicit def contTDeferInstane[F[_]: Defer, R]: Defer[ContT[F, R, *]] = new Defer[ContT[F, R, *]] {
    def defer[A](fa: => ContT[F, R, A]): ContT[F, R, A] = ContT.defer(fa)
  }
}
