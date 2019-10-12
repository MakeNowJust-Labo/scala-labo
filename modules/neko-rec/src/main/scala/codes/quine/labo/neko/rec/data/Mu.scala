package codes.quine.labo
package neko
package rec
package data

import neko.Functor, neko.syntax._

abstract class Mu[F[_]] extends Serializable {
  def unMu[A](f: F[A] => A): A
}

object Mu extends MuInstances {
  def apply[F[_]: Functor](ft: F[Mu[F]]): Mu[F] = Fix(ft, Functor[F])

  final private[data] case class Fix[F[_]](ft: F[Mu[F]], implicit val functor: Functor[F]) extends Mu[F] {
    def unMu[A](f: F[A] => A): A = f(ft.map(_.unMu(f)))
    override def toString: String = s"Mu($ft)"
  }
}

private[data] trait MuInstances {
  implicit def muBirecursiveInstance[F[_]: Functor]: Birecursive[F, Mu[F]] = new Birecursive[F, Mu[F]] {
    val functor: Functor[F] = Functor[F]

    def project(t: Mu[F]): F[Mu[F]] = t match {
      case Mu.Fix(ft, _) => ft
      case _             => lambek(t)
    }
    override def cata[A](t: Mu[F])(f: F[A] => A): A = t.unMu(f)

    def embed(ft: F[Mu[F]]): Mu[F] = Mu(ft)
  }
}
