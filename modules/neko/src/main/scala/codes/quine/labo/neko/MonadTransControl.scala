package codes.quine.labo
package neko

import data._

trait MonadTransControl[F[_[_], _], G[_]] extends MonadTransFunctor[F, G] {
  type State[A]

  def restore[A](s: State[A]): F[G, A]
  def zero[A](s: State[A]): Boolean
  def transControl[A](f: (F[G, *] ~> Lambda[A => G[State[A]]]) => G[A]): F[G, A]
}

object MonadTransControl {
  def apply[F[_[_], _], G[_]](implicit FG: MonadTransControl[F, G]): MonadTransControl[F, G] = FG
}
