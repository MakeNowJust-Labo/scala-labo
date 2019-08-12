package codes.quine.labo

import scalaprops._
import fun._

import neko._, data._, instances._, laws._, props._, syntax._

object FunProps extends Scalaprops {
  val prop = Property.forAllS[Fun[String, Int]] {
    case Fun(_, _, _, f) => f("monkey") == f("banana") && f("banana") == f("elephant")
  }

  val fold = Property.forAllS[(List[Int], Int), Fun[(Int, Int), Int]] {
    case ((xs, z), Fun(_, _, _, f)) =>
      xs.foldRight(z)(Function.untupled(f)) == xs.foldLeft(z)(Function.untupled(f))
  }

  val stateT = {
    implicit def stateTEqInstance[F[_], S, A](implicit ef: Eq[S => F[(S, A)]]): Eq[StateT[F, S, A]] = ef.by(_.run)
    implicit def stateTAlternativeInstance[F[_]: FlatMap: Alternative, S]: Alternative[StateT[F, S, *]] =
      new Alternative[StateT[F, S, *]] {
        def emptyK[A]: StateT[F, S, A] = StateT(_ => Alternative[F].emptyK)
        def concatK[A](x: StateT[F, S, A], y: StateT[F, S, A]): StateT[F, S, A] = StateT(s => x.run(s) <+> y.run(s))
        def pure[A](a: A): StateT[F, S, A] = StateT(s => Alternative[F].pure((s, a)))
        def ap[A, B](ff: StateT[F, S, A => B])(fa: StateT[F, S, A]): StateT[F, S, B] =
          ff.flatMap(f => fa.map(f)(FlatMap[F]))
      }

    implicit def funScalapropsGenInstance[A: Cogen: PartialFunArg, B: Gen]: Gen[Fun[A, B]] =
      Fun.gen(true)
    implicit def funScalapropsShrinkInstance[A, B: Shrink]: Shrink[Fun[A, B]] =
      Fun.shrink(true)

    type SO[R] = Fun[Boolean, Option[(Boolean, R)]]

    val laws = AlternativeLaws[StateT[Option, Boolean, *]](stateTAlternativeInstance[Option, Boolean])

    Property.forAllS[SO[Boolean], SO[Fun[Boolean, Boolean]], SO[Fun[Boolean, Boolean]]] { (fa0, ff0, fg0) =>
      val fa = StateT(fa0.f)
      val ff = StateT(ff0.f.andThen(_.map(((_: Fun[Boolean, Boolean]).f).second)))
      val fg = StateT(fg0.f.andThen(_.map(((_: Fun[Boolean, Boolean]).f).second)))
      laws.alternativeRightDistributivity(fa, ff, fg)
    }
  }
}
