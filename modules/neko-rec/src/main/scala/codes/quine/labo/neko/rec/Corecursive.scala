package codes.quine.labo
package neko
package rec

import neko.Functor, neko.syntax._

trait Corecursive[F[_], T] extends Serializable {
  implicit def functor: Functor[F]

  def embed(ft: F[T]): T

  def ana[A](a: A)(f: A => F[A]): T = {
    def af(a: A): T = embed(f(a).map(af))
    af(a)
  }

  def apo[A](a: A)(f: A => F[Either[T, A]]): T = {
    def af(a: A): T = embed(f(a).map(_.fold(t => t, af)))
    af(a)
  }
}

object Corecursive {
  def apply[F[_], T](implicit FT: Corecursive[F, T]): Corecursive[F, T] = FT
}
