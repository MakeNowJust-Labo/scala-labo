package codes.quine.labo
package neko

trait MonadState[F[_], S] {
  val monad: Monad[F]

  def get: F[S]
  def put(s: S): F[Unit]
  def inspect[A](f: S => A): F[A] = monad.map(get)(f)
  def modify(f: S => S): F[Unit] = monad.flatMap(get)(s => put(f(s)))
}

object MonadState {
  def apply[F[_], S](implicit state: MonadState[F, S]): MonadState[F, S] = state

  def get[F[_], S](implicit state: MonadState[F, S]): F[S] = state.get
  def put[F[_], S](s: S)(implicit state: MonadState[F, S]): F[Unit] = state.put(s)
  def inspect[F[_], S, A](f: S => A)(implicit state: MonadState[F, S]): F[A] = state.inspect(f)
  def modify[F[_], S](f: S => S)(implicit state: MonadState[F, S]): F[Unit] = state.modify(f)

  implicit def trans[F[_], G[_], S](implicit FG: MonadTransFunctor[F, G], state: MonadState[G, S]): MonadState[F, S] =
    new MonadState[F, S] {
      val monad: Monad[F] = FG.monad

      def get: F[S] = FG.lift(state.get)
      def put(s: S): F[Unit] = FG.lift(state.put(s))
    }
}
