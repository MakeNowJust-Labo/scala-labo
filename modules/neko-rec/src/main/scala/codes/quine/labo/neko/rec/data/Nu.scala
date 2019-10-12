package codes.quine.labo
package neko
package rec
package data

import neko.Functor, neko.syntax._

sealed abstract class Nu[F[_]] {
  type A
  def a: A
  def unNu(a: A): F[A]
}

object Nu extends NuInstances {
  type Aux[F[_], B] = Nu[F] { type A = B }

  def apply[F[_], B](b: B)(f: B => F[B]): Aux[F, B] = new Nu[F] {
    type A = B
    def a: B = b
    def unNu(a: B): F[B] = f(a)
  }
}

private[data] trait NuInstances {
  implicit def nuBirecursiveInstance[F[_]: Functor]: Birecursive[F, Nu[F]] = new Birecursive[F, Nu[F]] {
    val functor: Functor[F] = Functor[F]

    def project(t: Nu[F]): F[Nu[F]] = t.unNu(t.a).map(Nu(_)(t.unNu(_)))

    def embed(ft: F[Nu[F]]): Nu[F] = colambek(ft)
    override def ana[A](a: A)(f: A => F[A]): Nu[F] = Nu(a)(f)
  }
}
