package codes.quine.labo.nyan
package props

import laws._

import scalaprops._

trait ApplicativeProps[F[_]] extends FunctorProps[F] {
  val laws: ApplicativeLaws[F]

  implicit override val F: Applicative[F] = laws.F

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

  def applicative[A, B, C](implicit ga: Gen[A],
                           gfa: Gen[F[A]],
                           gf: Gen[A => B],
                           gff: Gen[F[A => B]],
                           gfg: Gen[F[B => C]],
                           efa: Eq[F[A]],
                           efb: Eq[F[B]],
                           efc: Eq[F[C]]): Properties[NyanLaw] =
    Properties
      .properties(NyanLaw.applicative)(
        NyanLaw.applicativeIdentity -> applicativeIdentity[A],
        NyanLaw.applicativeHomomorphism -> applicativeHomomorphism[A, B],
        NyanLaw.applicativeInterchange -> applicativeInterchange[A, B],
        NyanLaw.applicativeComposition -> applicativeComposition[A, B, C],
        NyanLaw.applicativeMap -> applicativeMap[A, B],
      )
      .andThenParam(Param.maxSize(20))
}

object ApplicativeProps {
  def apply[F[_]](implicit instance: Applicative[F]): ApplicativeProps[F] = new ApplicativeProps[F] {
    override lazy val laws: ApplicativeLaws[F] = ApplicativeLaws[F](instance)
  }
}
