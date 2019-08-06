package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._

object StateTProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def stateTGenInstance[F[_], S, A](implicit gf: Gen[S => F[(S, A)]]): Gen[StateT[F, S, A]] = gf.map(StateT(_))
  implicit def stateTEqInstance[F[_], S, A](implicit ef: Eq[S => F[(S, A)]]): Eq[StateT[F, S, A]] = ef.by(_.run)
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))
  implicit def idCogenInstance[A](implicit ca: Cogen[A]): Cogen[Id[A]] = Cogen[A].contramap(_.value)
  implicit def evalGenInstance[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def evalCogenInstance[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  val `laws (Id)` = Properties.list(
    MonadProps[State[Boolean, *]].all[Int, Int, Int]
  )

  val `laws (Eval)` = Properties.list(
    MonadProps[StateT[Eval, Boolean, *]].all[Int, Int, Int]
  )

  val `laws (Option)` = Properties.list(
    MonadProps[StateT[Option, Boolean, *]].all[Int, Int, Int],
    MonoidKProps[StateT[Option, Boolean, *]].all[Int],
    MonoidProps[StateT[List, Boolean, Boolean]].all
  )
}
