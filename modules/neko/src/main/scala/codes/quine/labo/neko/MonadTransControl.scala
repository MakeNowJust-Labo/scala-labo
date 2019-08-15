package codes.quine.labo
package neko

import data._

trait MonadTransControl[F[_], G[_]] extends MonadTransFunctor[F, G] {
  type State[A]
  type StateOfG[A] = G[State[A]]

  def restore[A](s: State[A]): F[A]
  def zero[A](s: State[A]): Boolean
  def transControl[A](f: (F ~> StateOfG) => G[A]): F[A]
}

object MonadTransControl {
  def apply[F[_], G[_]](implicit FG: MonadTransControl[F, G]): MonadTransControl[F, G] = FG
}
