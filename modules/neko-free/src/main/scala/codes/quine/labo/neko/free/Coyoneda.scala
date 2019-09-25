package codes.quine.labo.neko
package free

import data._

trait Coyoneda[F[_], A] { self =>
  type I
  def fi: F[I]
  def k: I => A

  def mapK[G[_]](t: F ~> G): Coyoneda[G, A] = new Coyoneda[G, A] {
    type I = self.I
    def fi: G[I] = t(self.fi)
    def k: I => A = self.k
  }

  def run(implicit F: Functor[F]): F[A] = F.map(fi)(k)
}

object Coyoneda {
  def lift[F[_], A](fa: F[A]): Coyoneda[F, A] = new Coyoneda[F, A] {
    type I = A
    def fi: F[I] = fa
    def k: A => A = identity
  }

  implicit def coyonedaFunctorInstance[F[_]]: Functor[Coyoneda[F, *]] = new Functor[Coyoneda[F, *]] {
    def map[A, B](fa: Coyoneda[F, A])(f: A => B): Coyoneda[F, B] = new Coyoneda[F, B] {
      type I = fa.I
      def fi: F[I] = fa.fi
      def k: I => B = fa.k.andThen(f)
    }
  }
}
