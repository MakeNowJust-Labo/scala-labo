package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait MonadProps[F[_]] {
  val laws: MonadLaws[F]
  import laws._

  def monadLeftIdentity[A, B](implicit ga: Gen[A], gf: Gen[A => F[B]], efb: Eq[F[B]]): Property =
    Property.forAll(laws.monadLeftIdentity(_: A, _: A => F[B]))

  def monadRightIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.monadRightIdentity(_: F[A]))

  def monadTailRecMStackSafety(implicit ef: Eq[F[Int]]): Property = Property.prop(laws.monadTailRecMStackSafety)

  def props[A, B](implicit ga: Gen[A],
                  gfa: Gen[F[A]],
                  gf: Gen[A => F[B]],
                  efa: Eq[F[A]],
                  efb: Eq[F[B]],
                  ef: Eq[F[Int]]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.monad)(
      NekoLaw.monadLeftIdentity -> monadLeftIdentity[A, B],
      NekoLaw.monadRightIdentity -> monadRightIdentity[A],
      NekoLaw.monadTailRecMStackSafety -> monadTailRecMStackSafety
    )

  def all[A, B, C](implicit ga: Gen[A],
                   gfa: Gen[F[A]],
                   gf0: Gen[A => B],
                   gg0: Gen[B => C],
                   gf: Gen[A => F[B]],
                   gg: Gen[B => F[C]],
                   gff: Gen[F[A => B]],
                   gfg: Gen[F[B => C]],
                   efa: Eq[F[A]],
                   efb: Eq[F[B]],
                   efc: Eq[F[C]],
                   ef: Eq[F[Int]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.monadAll, ApplicativeProps[F].all[A, B, C], FlatMapProps[F].all[A, B, C], props[A, B])
}

object MonadProps {
  def apply[F[_]: Monad]: MonadProps[F] = new MonadProps[F] {
    val laws: MonadLaws[F] = MonadLaws[F]
  }
}
