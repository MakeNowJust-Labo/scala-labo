package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait AlternativeProps[F[_]] {
  val laws: AlternativeLaws[F]
  import laws._

  def alternativeRightAbsorption[A, B](implicit gff: Gen[F[A => B]], efb: Eq[F[B]]): Property =
    Property.forAll(laws.alternativeRightAbsorption(_: F[A => B]))

  def alternativeLeftDistributivity[A, B](implicit gfa: Gen[F[A]], gf: Gen[A => B], efb: Eq[F[B]]): Property =
    Property.forAll(laws.alternativeLeftDistributivity(_: F[A], _: F[A], _: A => B))

  def alternativeRightDistributivity[A, B](implicit gfa: Gen[F[A]], gff: Gen[F[A => B]], efb: Eq[F[B]]): Property =
    Property.forAll(laws.alternativeRightDistributivity(_: F[A], _: F[A => B], _: F[A => B]))

  def props[A, B](implicit gfa: Gen[F[A]], gf: Gen[A => B], gff: Gen[F[A => B]], efb: Eq[F[B]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.alternative)(
      NekoLaw.alternativeRightAbsorption -> alternativeRightAbsorption[A, B],
      NekoLaw.alternativeLeftDistributivity -> alternativeLeftDistributivity[A, B],
      NekoLaw.alternativeRightDistributivity -> alternativeRightDistributivity[A, B]
    )

  def all[A, B, C](implicit ga: Gen[A],
                   gfa: Gen[F[A]],
                   gf: Gen[A => B],
                   gg: Gen[B => C],
                   gff: Gen[F[A => B]],
                   gfg: Gen[F[B => C]],
                   efa: Eq[F[A]],
                   efb: Eq[F[B]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.alternativeAll, ApplicativeProps[F].all[A, B, C], MonoidKProps[F].all[A], props[A, B])
}

object AlternativeProps {
  def apply[F[_]: Alternative]: AlternativeProps[F] = new AlternativeProps[F] {
    val laws: AlternativeLaws[F] = AlternativeLaws[F]
  }
}
