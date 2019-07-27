package codes.quine.labo.neko
package props

import laws._

import scalaprops._

trait AlternativeProps[F[_]] extends ApplicativeProps[F] with MonoidKProps[F] {
  val laws: AlternativeLaws[F]

  implicit override val F: Alternative[F] = laws.F

  def alternativeRightAbsorption[A, B](implicit gff: Gen[F[A => B]], efb: Eq[F[B]]): Property =
    Property.forAll(laws.alternativeRightAbsorption(_: F[A => B]))

  def alternativeLeftDistributivity[A, B](implicit gfa: Gen[F[A]], gf: Gen[A => B], efb: Eq[F[B]]): Property =
    Property.forAll(laws.alternativeLeftDistributivity(_: F[A], _: F[A], _: A => B))

  def alternativeRightDistributivity[A, B](implicit gfa: Gen[F[A]], gff: Gen[F[A => B]], efb: Eq[F[B]]): Property =
    Property.forAll(laws.alternativeRightDistributivity(_: F[A], _: F[A => B], _: F[A => B]))

  def alternative[A, B](implicit gfa: Gen[F[A]],
                        gf: Gen[A => B],
                        gff: Gen[F[A => B]],
                        efb: Eq[F[B]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.alternative)(
      NekoLaw.alternativeRightAbsorption -> alternativeRightAbsorption[A, B],
      NekoLaw.alternativeLeftDistributivity -> alternativeLeftDistributivity[A, B],
      NekoLaw.alternativeRightDistributivity -> alternativeRightDistributivity[A, B]
    )
}

object AlternativeProps {
  def apply[F[_]](implicit instance: Alternative[F]): AlternativeProps[F] = new AlternativeProps[F] {
    override lazy val laws: AlternativeLaws[F] = AlternativeLaws[F](instance)
  }
}