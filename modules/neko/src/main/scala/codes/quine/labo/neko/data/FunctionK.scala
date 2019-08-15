package codes.quine.labo
package neko
package data

trait FunctionK[F[_], G[_]] { t =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](s: FunctionK[E, F]): E ~> G =
    Lambda[E ~> G](fx => t(s(fx)))

  def andThen[H[_]](u: FunctionK[G, H]): F ~> H =
    Lambda[F ~> H](fx => u(t(fx)))
}

object FunctionK {
  def identity[F[_]]: F ~> F = Lambda[F ~> F](fx => fx)
}
