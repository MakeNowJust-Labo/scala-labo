package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait ComonadProps[F[_]] {
  val laws: ComonadLaws[F]
  import laws._

  def comonadExtractCoflattenIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.comonadExtractCoflattenIdentity(_: F[A]))

  def comonadMapCoflattenIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.comonadMapCoflattenIdentity(_: F[A]))

  def comonadMapCoflatMapCoherence[A, B](implicit gfa: Gen[F[A]], gf: Gen[A => B], efb: Eq[F[B]]): Property =
    Property.forAll(laws.comonadMapCoflatMapCoherence(_: F[A], _: A => B))

  def comonadLeftIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.comonadLeftIdentity(_: F[A]))

  def comonadRightIdentity[A, B](implicit gfa: Gen[F[A]], gf: Gen[F[A] => B], eb: Eq[B]): Property =
    Property.forAll(laws.comonadRightIdneity(_: F[A], _: F[A] => B))

  def props[A, B](implicit gfa: Gen[F[A]],
                  gf0: Gen[A => B],
                  gf: Gen[F[A] => B],
                  efa: Eq[F[A]],
                  eb: Eq[B],
                  efb: Eq[F[B]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.comonad)(
      NekoLaw.comonadExtractCoflattenIdentity -> comonadExtractCoflattenIdentity[A],
      NekoLaw.comonadMapCoflattenIdentity -> comonadMapCoflattenIdentity[A],
      NekoLaw.comonadMapCoflatMapCoherence -> comonadMapCoflatMapCoherence[A, B],
      NekoLaw.comonadLeftIdentity -> comonadLeftIdentity[A],
      NekoLaw.comonadRightIdnetity -> comonadRightIdentity[A, B]
    )

  def all[A, B, C](implicit gfa: Gen[F[A]],
                   gf0: Gen[A => B],
                   gg0: Gen[B => C],
                   gf: Gen[F[A] => B],
                   gg: Gen[F[B] => C],
                   efa: Eq[F[A]],
                   effa: Eq[F[F[A]]],
                   efffa: Eq[F[F[F[A]]]],
                   eb: Eq[B],
                   efb: Eq[F[B]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.comonadAll, CoflatMapProps[F].all[A, B, C], props[A, B])
}

object ComonadProps {
  def apply[F[_]: Comonad]: ComonadProps[F] = new ComonadProps[F] {
    val laws: ComonadLaws[F] = ComonadLaws[F]
  }
}
