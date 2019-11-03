package codes.quine.labo
package fun

import scalaprops._

import neko._, data._, instances._, laws._, props._, syntax._

object FunProps extends Scalaprops {
  val prop = Property
    .forAllS[Fun[String, Int]] {
      case Fun(_, _, _, f) => f("あいうえお") == f("かきくけこ") && f("かきくけこ") == f("さしすせそ")
    }
    .ignore("it is example for Fun")

  val fold = Property
    .forAllS[(List[Int], Int), Fun[(Int, Int), Int]] {
      case ((xs, z), Fun(_, _, _, f)) =>
        xs.foldRight(z)(Function.untupled(f)) == xs.foldLeft(z)(Function.untupled(f))
    }
    .ignore("it is example for Fun")

  val byte = Property
    .forAllS[Fun[(Byte, List[Byte]), Int]] {
      case Fun(_, _, _, f) => f((1, List(127, 2))) == f((2, List(2))) && f((2, List(2))) == f((1, List(1, 2, 3)))
    }
    .ignore("it is example for Fun")

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
  }.ignore("it is example for Fun")

  // Small result:
  //
  // 1. Fun.from(false -> None)(Some((false,false)))
  // 2. Fun.from(false -> None)(Some((false,Fun.from(false -> true, true -> true)(true))))
  // 3. Fun.from(false -> None)(Some((true,Fun.from(false -> false, true -> true)(false)))
}
