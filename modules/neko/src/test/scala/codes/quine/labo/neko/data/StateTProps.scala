package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._, syntax._

object StateTProps extends Scalaprops {
  implicit def stateTGenInstance[F[_], S, A](implicit gf: Gen[S => F[(S, A)]]): Gen[StateT[F, S, A]] = gf.map(StateT(_))
  implicit def stateTEqInstance[F[_], S, A](implicit ef: Eq[S => F[(S, A)]]): Eq[StateT[F, S, A]] = ef.by(_.run)
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))
  implicit def idCogenInstance[A](implicit ca: Cogen[A]): Cogen[Id[A]] = Cogen[A].contramap(_.value)
  implicit def evalGenInstance[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def evalCogenInstance[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  // TODO: add `Tuple2Instances`
  implicit def tuple2EqInstance[A: Eq, B: Eq]: Eq[(A, B)] = Eq.eqv {
    case ((a1, b1), (a2, b2)) => a1 === a2 && b1 === b2
  }

  val `laws (Id)` = Properties.list(
    MonadProps[State[Boolean, *]].all[Int, Int, Int]
  )

  val `laws (Eval)` = Properties.list(
    MonadProps[StateT[Eval, Boolean, *]].all[Int, Int, Int]
  )

  val `laws (List)` = Properties.list(
    MonadProps[StateT[List, Boolean, *]].all[Int, Int, Int],
    AlternativeProps[StateT[List, Boolean, *]].all[Int, Int, Int]
  )
}
