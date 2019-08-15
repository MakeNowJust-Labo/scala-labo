package codes.quine.labo
package neko

import data._

trait MonadTransFunctor[F[_[_], _], G[_]] extends MonadTrans[F, G] {
  def transMap[A](fa: F[G, A])(t: G ~> G): F[G, A]

  override def trans[A](fa: F[G, A])(t: G ~> G, u: G ~> G): F[G, A] = transMap(fa)(t)
}

object MonadTransFunctor {
  def apply[F[_[_], _], G[_]](implicit FG: MonadTransFunctor[F, G]): MonadTransFunctor[F, G] = FG
}
