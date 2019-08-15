package codes.quine.labo
package neko

trait MonadWriter[F[_], L] {
  val monad: Monad[F]

  def tell(l: L): F[Unit]
  def listen[A](fa: F[A]): F[(L, A)]
}

object MonadWriter {
  def apply[F[_], L](implicit writer: MonadWriter[F, L]): MonadWriter[F, L] = writer

  def tell[F[_], L](l: L)(implicit writer: MonadWriter[F, L]): F[Unit] = writer.tell(l)
  def listen[F[_], L, A](fa: F[A])(implicit writer: MonadWriter[F, L]): F[(L, A)] = writer.listen(fa)

  implicit def trans[F[_[_], _], G[_], L](implicit FG: MonadTransControl[F, G],
                                          writer: MonadWriter[G, L]): MonadWriter[F[G, *], L] =
    new MonadWriter[F[G, *], L] {
      val monad = FG.monad

      def tell(l: L): F[G, Unit] = FG.lift(writer.tell(l))
      def listen[A](fa: F[G, A]): F[G, (L, A)] =
        monad.flatMap(FG.transControl(k => writer.listen(k(fa)))) {
          case (l, s) => monad.map(FG.restore(s))(a => (l, a))
        }
    }
}
