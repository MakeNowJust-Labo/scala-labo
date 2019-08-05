package codes.quine.labo
package neko
package data

import scalaprops._
import instances._, props._

object ContTProps extends Scalaprops {
  implicit def contTGenInstance[F[_], R, A](implicit gk: Gen[(A => F[R]) => F[R]]): Gen[ContT[F, R, A]] = gk.map(ContT(_))
  implicit def contTApplicativeEqInstance[F[_]: Applicative, R](implicit efr: Eq[F[R]]): Eq[ContT[F, R, R]] = efr.by(_.run(Applicative[F].pure(_)))
  implicit def evalGenInstance[A: Gen]: Gen[Eval[A]] = Gen[A].map(Eval.now(_))
  implicit def evalCogenInstance[A: Cogen]: Cogen[Eval[A]] = Cogen[A].contramap(_.value)

  val laws = Properties.list(
    MonadProps[Cont[Int, *]].all[Int, Int, Int],
    DeferProps[Cont[Int, *]].all[Int],
  )
}