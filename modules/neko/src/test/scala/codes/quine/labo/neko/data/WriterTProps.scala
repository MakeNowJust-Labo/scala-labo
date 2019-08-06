package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._

object WriterTProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def writerTGenInstance[F[_], L, A](implicit gf: Gen[F[(L, A)]]): Gen[WriterT[F, L, A]] = gf.map(WriterT(_))
  implicit def writerTCogenInstance[F[_], L, A](implicit cf: Cogen[F[(L, A)]]): Cogen[WriterT[F, L, A]] =
    cf.contramap(_.run)
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))
  implicit def idCogenInstance[A](implicit ca: Cogen[A]): Cogen[Id[A]] = Cogen[A].contramap(_.value)
  implicit def evalGenInstance[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def evalCogenInstance[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  val `laws (Id)` = Properties.list(
    OrdProps[Writer[String, Int]].all,
    HashProps[Writer[String, Int]].all,
    MonoidProps[Writer[String, String]].all,
    MonadProps[Writer[String, *]].all[Int, Int, Int]
  )

  val `laws (Eval)` = Properties.list(
    OrdProps[WriterT[Eval, String, Int]].all,
    HashProps[WriterT[Eval, String, Int]].all,
    MonoidProps[WriterT[Eval, String, String]].all,
    MonadProps[WriterT[Eval, String, *]].all[Int, Int, Int]
  )

  val `laws (Option)` = Properties.list(
    OrdProps[WriterT[Option, String, Int]].all,
    HashProps[WriterT[Option, String, Int]].all,
    MonoidProps[WriterT[Option, String, String]].all,
    MonadProps[WriterT[Option, String, *]].all[Int, Int, Int],
    AlternativeProps[WriterT[Option, String, *]].all[Int, Int, Int]
  )
}
