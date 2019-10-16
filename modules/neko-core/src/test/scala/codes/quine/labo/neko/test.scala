package codes.quine.labo
package neko

import scalaprops._
import data._, instances._

object test {
  implicit def readerTEqInstance[F[_], E, A](implicit ef: Eq[E => F[A]]): Eq[ReaderT[F, E, A]] = ef.by(_.run)
  implicit def contTApplicativeEqInstance[F[_]: Applicative, R](implicit efr: Eq[F[R]]): Eq[ContT[F, R, R]] =
    efr.by(_.run(Applicative[F].pure(_)))

  implicit val booleanMonoidInstance: Monoid[Boolean] = boolean.allMonoidInstance

  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def constGenInstance[A: Gen, B]: Gen[Const[A, B]] = Gen[A].map(Const(_))
  implicit def constCogenInstance[A: Cogen, B]: Cogen[Const[A, B]] = Cogen[A].contramap(_.value)
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))
  implicit def idCogenInstance[A](implicit ca: Cogen[A]): Cogen[Id[A]] = Cogen[A].contramap(_.value)
  implicit def nestedGenInstance[F[_], G[_], A](implicit gfga: Gen[F[G[A]]]): Gen[Nested[F, G, A]] =
    Gen[F[G[A]]].map(Nested(_))
  implicit def nestedCogenInstance[F[_], G[_], A](implicit cfga: Cogen[F[G[A]]]): Cogen[Nested[F, G, A]] =
    Cogen[F[G[A]]].contramap(_.value)
  implicit def contTGenInstance[F[_], R, A](implicit gk: Gen[(A => F[R]) => F[R]]): Gen[ContT[F, R, A]] =
    gk.map(ContT(_))
  implicit def evalGenInstance[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def evalCogenInstance[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)
  implicit val orderingGenInstance: Gen[Ordering] = Gen[Int].map(Ordering.fromInt(_))
  implicit val orderingCogenInstance: Cogen[Ordering] = Cogen[Int].contramap(_.toInt)
  implicit def readerTGenInstance[F[_], E, A](implicit gf: Gen[E => F[A]]): Gen[ReaderT[F, E, A]] = gf.map(ReaderT(_))
  implicit def stateTGenInstance[F[_], S, A](implicit gf: Gen[S => F[(S, A)]]): Gen[StateT[F, S, A]] = gf.map(StateT(_))
  implicit def stateTEqInstance[F[_], S, A](implicit ef: Eq[S => F[(S, A)]]): Eq[StateT[F, S, A]] = ef.by(_.run)
  implicit def writerTGenInstance[F[_], L, A](implicit gf: Gen[F[(L, A)]]): Gen[WriterT[F, L, A]] = gf.map(WriterT(_))
  implicit def writerTCogenInstance[F[_], L, A](implicit cf: Cogen[F[(L, A)]]): Cogen[WriterT[F, L, A]] =
    cf.contramap(_.run)
}
