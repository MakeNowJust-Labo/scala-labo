package codes.quine.labo
package neko

import data._

trait MonadTrans[F[_[_], _], G[_]] {
  val monad: Monad[F[G, *]]
  val innerMonad: Monad[G]

  def lift[A](ga: G[A]): F[G, A]
  def trans[A](fa: F[G, A])(t: G ~> G, u: G ~> G): F[G, A]
}

object MonadTrans {
  def apply[F[_[_], _], G[_]](implicit FG: MonadTrans[F, G]): MonadTrans[F, G] = FG
}
