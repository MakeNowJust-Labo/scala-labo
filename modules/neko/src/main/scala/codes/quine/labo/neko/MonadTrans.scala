package codes.quine.labo
package neko

import data._

trait MonadTrans[F[_], G[_]] {
  val monad: Monad[F]
  val innerMonad: Monad[G]

  def lift[A](ga: G[A]): F[A]
  def trans[A](fa: F[A])(t: G ~> G, u: G ~> G): F[A]
}

object MonadTrans {
  def apply[F[_], G[_]](implicit FG: MonadTrans[F, G]): MonadTrans[F, G] = FG
}
