package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._

object ReaderTProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def readerTGenInstance[F[_], E, A](implicit gf: Gen[E =>F[A]]): Gen[ReaderT[F, E, A]] = gf.map(ReaderT(_))
  implicit def readerTEqInstance[F[_], E, A](implicit ef: Eq[E => F[A]]): Eq[ReaderT[F, E, A]] = ef.by(_.run)
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))
  implicit def idCogenInstance[A](implicit ca: Cogen[A]): Cogen[Id[A]] = Cogen[A].contramap(_.value)
  implicit def evalGenInstance[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def evalCogenInstance[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  val `laws (Id)` = Properties.list(
    MonadProps[Reader[Boolean, *]].all[Int, Int, Int],
    MonoidProps[Reader[Boolean, String]].all,
  )

  val `laws (Eval)` = Properties.list(
    MonadProps[ReaderT[Eval, Boolean, *]].all[Int, Int, Int],
    MonoidProps[ReaderT[Eval, Boolean, String]].all,
  )

  val `laws (List)` = Properties.list(
    MonadProps[ReaderT[List, Boolean, *]].all[Int, Int, Int],
    AlternativeProps[ReaderT[List, Boolean, *]].all[Int, Int, Int],
    MonoidProps[ReaderT[List, Boolean, Int]].all,
  )
}