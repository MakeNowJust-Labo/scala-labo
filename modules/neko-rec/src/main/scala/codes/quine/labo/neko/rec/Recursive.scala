package codes.quine.labo
package neko
package rec

import neko.Functor, neko.syntax._

trait Recursive[F[_], T] extends Serializable {
  implicit def functor: Functor[F]

  def project(t: T): F[T]

  def cata[A](t: T)(f: F[A] => A): A = {
    def cf(t: T): A = f(project(t).map(cf))
    cf(t)
  }

  def para[A](t: T)(f: F[(T, A)] => A): A = {
    def pf(t: T): A = f(project(t).map(t1 => (t1, pf(t1))))
    pf(t)
  }
}

object Recursive {
  def apply[F[_], T](implicit FT: Recursive[F, T]): Recursive[F, T] = FT
}
