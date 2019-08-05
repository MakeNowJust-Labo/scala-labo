package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait BimonadProps[F[_]] {
  val laws: BimonadLaws[F]
  import laws._

  def bimonadExtractPureIdentity[A](implicit ga: Gen[A], ea: Eq[A]): Property =
    Property.forAll(laws.bimonadExtractPureIdentity(_: A))

  def bimonadExtractFlatMapEntwining[A](implicit gffa: Gen[F[F[A]]], ea: Eq[A]): Property =
    Property.forAll(laws.bimonadExtractFlatMapEntwining(_: F[F[A]]))

  def bimonadPureCoflatMapEntwining[A](implicit ga: Gen[A], effa: Eq[F[F[A]]]): Property =
    Property.forAll(laws.bimonadPureCoflatMapEntwining(_: A))

  def props[A](implicit ga: Gen[A], gffa: Gen[F[F[A]]], ea: Eq[A], effa: Eq[F[F[A]]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.bimonad)(
      NekoLaw.bimonadExtractPureIdentity -> bimonadExtractPureIdentity[A],
      NekoLaw.bimonadExtractFlatMapEntwining -> bimonadExtractFlatMapEntwining[A],
      NekoLaw.bimonadPureCoflatMapEntwining -> bimonadPureCoflatMapEntwining[A]
    )

  def all[A, B, C](implicit ga: Gen[A],
                   gfa: Gen[F[A]],
                   gffa: Gen[F[F[A]]],
                   gf0: Gen[A => B],
                   gg0: Gen[B => C],
                   gf: Gen[A => F[B]],
                   gg: Gen[B => F[C]],
                   gf1: Gen[F[A] => B],
                   gg1: Gen[F[B] => C],
                   gff: Gen[F[A => B]],
                   gfg: Gen[F[B => C]],
                   ea: Eq[A],
                   efa: Eq[F[A]],
                   effa: Eq[F[F[A]]],
                   efffa: Eq[F[F[F[A]]]],
                   eb: Eq[B],
                   efb: Eq[F[B]],
                   efc: Eq[F[C]],
                   ef: Eq[F[Int]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.bimonadAll, MonadProps[F].all[A, B, C], ComonadProps[F].all[A, B, C], props[A])
}

object BimonadProps {
  def apply[F[_]: Bimonad]: BimonadProps[F] = new BimonadProps[F] {
    val laws: BimonadLaws[F] = BimonadLaws[F]
  }
}
