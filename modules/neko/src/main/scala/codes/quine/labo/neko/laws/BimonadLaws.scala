package codes.quine.labo
package neko
package laws

import syntax._

trait BimonadLaws[F[_]] {
  implicit val F: Bimonad[F]

  def bimonadExtractPureIdentity[A](a: A): IsEq[A] =
    a <-> F.pure(a).extract

  def bimonadExtractFlatMapEntwining[A](ffa: F[F[A]]): IsEq[A] =
    ffa.flatten.extract <-> ffa.map(_.extract).extract

  def bimonadPureCoflatMapEntwining[A](a: A): IsEq[F[F[A]]] =
    F.pure(a).coflatten <-> F.pure(a).map(F.pure(_))
}

object BimonadLaws {
  def apply[F[_]: Bimonad]: BimonadLaws[F] = new BimonadLaws[F] {
    val F: Bimonad[F] = Bimonad[F]
  }
}
