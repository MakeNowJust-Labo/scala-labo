package codes.quine.labo
package neko
package props

import scalaprops._
import laws._

trait ApplicativeProps[F[_]] {
  val laws: ApplicativeLaws[F]
  import laws._

  def applicativeIdentity[A](implicit gfa: Gen[F[A]], efa: Eq[F[A]]): Property =
    Property.forAll(laws.applicativeIdentity(_: F[A]))

  def applicativeHomomorphism[A, B](implicit ga: Gen[A], gf: Gen[A => B], efb: Eq[F[B]]): Property =
    Property.forAll(laws.applicativeHomomorphism(_: A, _: A => B))

  def applicativeInterchange[A, B](implicit ga: Gen[A], gff: Gen[F[A => B]], efb: Eq[F[B]]): Property =
    Property.forAll(laws.applicativeInterchange(_: A, _: F[A => B]))

  def applicativeMap[A, B](implicit gfa: Gen[F[A]], gf: Gen[A => B], gfb: Eq[F[B]]): Property =
    Property.forAll(laws.applicativeMap(_: F[A], _: A => B))

  def applicativeComposition[A, B, C](implicit gfa: Gen[F[A]],
                                      gff: Gen[F[A => B]],
                                      gfg: Gen[F[B => C]],
                                      efc: Eq[F[C]]): Property =
    Property.forAll(laws.applicativeComposition(_: F[A], _: F[A => B], _: F[B => C]))

  def props[A, B, C](implicit ga: Gen[A],
                     gfa: Gen[F[A]],
                     gf: Gen[A => B],
                     gff: Gen[F[A => B]],
                     gfg: Gen[F[B => C]],
                     efa: Eq[F[A]],
                     efb: Eq[F[B]],
                     efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties
      .properties(NekoLaw.applicative)(
        NekoLaw.applicativeIdentity -> applicativeIdentity[A],
        NekoLaw.applicativeHomomorphism -> applicativeHomomorphism[A, B],
        NekoLaw.applicativeInterchange -> applicativeInterchange[A, B],
        NekoLaw.applicativeComposition -> applicativeComposition[A, B, C],
        NekoLaw.applicativeMap -> applicativeMap[A, B]
      )
      .andThenParam(Param.maxSize(20))

  def all[A, B, C](implicit ga: Gen[A],
                   gfa: Gen[F[A]],
                   gf: Gen[A => B],
                   gg: Gen[B => C],
                   gff: Gen[F[A => B]],
                   gfg: Gen[F[B => C]],
                   efa: Eq[F[A]],
                   efb: Eq[F[B]],
                   efc: Eq[F[C]]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.applicativeAll, FunctorProps[F].all[A, B, C], props[A, B, C])
}

object ApplicativeProps {
  def apply[F[_]: Applicative]: ApplicativeProps[F] = new ApplicativeProps[F] {
    val laws: ApplicativeLaws[F] = ApplicativeLaws[F]
  }
}
