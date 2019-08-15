package codes.quine.labo
package neko

import data._

trait MonadReader[F[_], E] {
  val monad: Monad[F]

  def ask: F[E]
  def local[A](f: E => E)(fa: F[A]): F[A]
}

object MonadReader {
  def apply[F[_], E](implicit reader: MonadReader[F, E]): MonadReader[F, E] = reader

  def ask[F[_], E](implicit reader: MonadReader[F, E]): F[E] = reader.ask
  def local[F[_], E, A](f: E => E)(fa: F[A])(implicit reader: MonadReader[F, E]): F[A] = reader.local(f)(fa)

  implicit def trans[F[_[_], _], G[_], E](implicit FG: MonadTrans[F, G],
                                          reader: MonadReader[G, E]): MonadReader[F[G, *], E] =
    new MonadReader[F[G, *], E] {
      val monad: Monad[F[G, *]] = FG.monad

      def ask: F[G, E] = FG.lift(reader.ask)
      def local[A](f: E => E)(fa: F[G, A]): F[G, A] =
        monad.flatMap(ask) { r =>
          FG.trans(fa)(
            Lambda[G ~> G](reader.local(f)(_)),
            Lambda[G ~> G](reader.local(_ => r)(_))
          )
        }
    }
}
