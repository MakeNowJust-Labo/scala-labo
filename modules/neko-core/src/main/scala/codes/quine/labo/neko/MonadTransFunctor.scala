package codes.quine.labo
package neko

import data._

trait MonadTransFunctor[F[_], G[_]] extends MonadTrans[F, G] {
  def transMap[A](fa: F[A])(t: G ~> G): F[A]

  override def trans[A](fa: F[A])(t: G ~> G, u: G ~> G): F[A] = transMap(fa)(t)
}

object MonadTransFunctor {
  def apply[F[_], G[_]](implicit FG: MonadTransFunctor[F, G]): MonadTransFunctor[F, G] = FG
}
