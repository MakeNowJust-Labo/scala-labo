package codes.quine.labo

import com.github.ghik.silencer.silent
import scalaprops.{Cogen, Gen, Shrink}
import simulacrum.typeclass
import scala.reflect.ClassTag

sealed abstract class PartialFun[A, B] {
  def isEmpty: Boolean
  def map[C](f: B => C): PartialFun[A, C]
  def table: LazyList[(A, B)]
  def toFunction(d: B): A => B
  def shrink(s: B => LazyList[B]): LazyList[PartialFun[A, B]]
}

object PartialFun {
  def apply[A: PartialFunArg, B](f: A => B): PartialFun[A, B] = PartialFunArg[A].build(f)

  def empty[A, B]: PartialFun[A, B] = Empty[A, B]
  def point[B](b: B): PartialFun[Unit, B] = Point(b)
  def uncurry[A, B, C](abc: PartialFun[A, PartialFun[B, C]]): PartialFun[(A, B), C] =
    if (abc.isEmpty) Empty[(A, B), C] else Uncurry(abc)
  def choice[A, B, C](ac: PartialFun[A, C], bc: PartialFun[B, C]): PartialFun[Either[A, B], C] =
    if (ac.isEmpty && bc.isEmpty) Empty[Either[A, B], C] else Choice(ac, bc)
  def iso[A, B, C](embed: A => B, eject: B => A, bc: PartialFun[B, C]): PartialFun[A, C] =
    if (bc.isEmpty) Empty[A, C] else Iso(embed, eject, bc)

  final class Empty[A, B] private () extends PartialFun[A, B] {
    def isEmpty: Boolean = true
    def map[C](f: B => C): PartialFun[A, C] = Empty[A, C]
    def table: LazyList[(A, B)] = LazyList.empty
    def toFunction(d: B): A => B = _ => d
    def shrink(s: B => LazyList[B]): LazyList[PartialFun[A, B]] = LazyList.empty

    override def toString: String = "Empty"
  }

  object Empty {
    private[this] val instance: Empty[Any, Any] = new Empty[Any, Any]
    def apply[A, B]: Empty[A, B] = instance.asInstanceOf[Empty[A, B]]
  }

  final class Point[B] private (b: Lazy[B]) extends PartialFun[Unit, B] {
    def isEmpty: Boolean = false
    def map[C](f: B => C): PartialFun[Unit, C] = Point(f(b.value))
    def table: LazyList[(Unit, B)] = LazyList.cons(() -> b.value, LazyList.empty)
    def toFunction(d: B): Unit => B = _ => b.value
    def shrink(s: B => LazyList[B]): LazyList[PartialFun[Unit, B]] = s(b.value).map(point(_))

    override def toString: String = s"Point($b)"
  }

  object Point {
    def apply[B](b: => B): Point[B] = new Point(Lazy(b))
  }

  final class Uncurry[A, B, C] private (abc: Lazy[PartialFun[A, PartialFun[B, C]]]) extends PartialFun[(A, B), C] {
    def isEmpty: Boolean = false
    def map[D](f: C => D): PartialFun[(A, B), D] = Uncurry(abc.value.map(_.map(f)))
    def table: LazyList[((A, B), C)] =
      abc.value.table.flatMap { case (a, bc) => bc.table.map { case (b, c) => ((a, b), c) } }
    def toFunction(d: C): ((A, B)) => C = { case (a, b) => abc.value.map(_.toFunction(d)(b)).toFunction(d)(a) }
    def shrink(s: C => LazyList[C]): LazyList[PartialFun[(A, B), C]] =
      abc.value.shrink(bc => bc.shrink(s)).map(uncurry(_))

    override def toString: String = s"Uncurry($abc)"
  }

  object Uncurry {
    def apply[A, B, C](abc: PartialFun[A, PartialFun[B, C]]): Uncurry[A, B, C] = new Uncurry(Lazy(abc))
  }

  final class Choice[A, B, C] private (ac: Lazy[PartialFun[A, C]], bc: Lazy[PartialFun[B, C]])
      extends PartialFun[Either[A, B], C] {
    def isEmpty: Boolean = false
    def map[D](f: C => D): PartialFun[Either[A, B], D] = Choice(ac.value.map(f), bc.value.map(f))
    def table: LazyList[(Either[A, B], C)] =
      ac.value.table.map { case (a, c) => (Left(a), c) } ++ bc.value.table.map { case (b, c) => (Right(b), c) }
    def toFunction(d: C): Either[A, B] => C = {
      lazy val f = ac.value.toFunction(d)
      lazy val g = bc.value.toFunction(d)
      (_: Either[A, B]) match {
        case Left(a)  => f(a)
        case Right(b) => g(b)
      }
    }
    def shrink(s: C => LazyList[C]): LazyList[PartialFun[Either[A, B], C]] =
      (if (!ac.value.isEmpty && !bc.value.isEmpty)
         LazyList(choice(ac.value, Empty[B, C]), choice(Empty[A, C], bc.value))
       else LazyList.empty) #:::
        bc.value.shrink(s).map(choice(ac.value, _)) #::: ac.value.shrink(s).map(choice(_, bc.value))

    override def toString: String = s"Choice($ac, $bc)"
  }

  object Choice {
    def apply[A, B, C](ac: => PartialFun[A, C], bc: => PartialFun[B, C]): Choice[A, B, C] =
      new Choice(Lazy(ac), Lazy(bc))
  }

  final class Iso[A, B, C] private (val embed: A => B, val eject: B => A, bc: Lazy[PartialFun[B, C]])
      extends PartialFun[A, C] {
    def isEmpty: Boolean = false
    def map[D](f: C => D): PartialFun[A, D] = Iso(embed, eject, bc.value.map(f))
    def table: LazyList[(A, C)] =
      bc.value.table.map { case (b, c) => (eject(b), c) }
    def toFunction(d: C): A => C = {
      lazy val g = bc.value.toFunction(d)
      (a: A) => g(embed(a))
    }
    def shrink(s: C => LazyList[C]): LazyList[PartialFun[A, C]] =
      bc.value.shrink(s).map(iso(embed, eject, _))

    override def toString: String = s"Iso(<embed>, <eject>, $bc)"
  }

  object Iso {
    def apply[A, B, C](embed: A => B, eject: B => A, bc: => PartialFun[B, C]): Iso[A, B, C] =
      new Iso(embed, eject, Lazy(bc))

    def build[A, B, C](embed: A => B, eject: B => A, f: A => C)(implicit B: PartialFunArg[B]): Iso[A, B, C] =
      Iso(embed, eject, B.build(b => f(eject(b))))
  }
}

@typeclass trait PartialFunArg[A] { self =>
  import PartialFun._

  def build[B](f: A => B): PartialFun[A, B]

  @simulacrum.noop
  def by[X](embed: X => A, eject: A => X): PartialFunArg[X] = new PartialFunArg[X] {
    def build[B](f: X => B): PartialFun[X, B] =
      Iso.build(embed, eject, f)(self)
  }
}

object PartialFunArg {
  import PartialFun._

  implicit val unit: PartialFunArg[Unit] = new PartialFunArg[Unit] {
    def build[B](f: Unit => B): PartialFun[Unit, B] = Point(f(()))
  }

  implicit def either[A, B](implicit A: PartialFunArg[A], B: PartialFunArg[B]): PartialFunArg[Either[A, B]] =
    new PartialFunArg[Either[A, B]] {
      def build[C](f: Either[A, B] => C): PartialFun[Either[A, B], C] =
        Choice(A.build(a => f(Left(a))), B.build(b => f(Right(b))))
    }

  implicit def pair[A, B](implicit A: PartialFunArg[A], B: PartialFunArg[B]): PartialFunArg[(A, B)] =
    new PartialFunArg[(A, B)] {
      def build[C](f: ((A, B)) => C): PartialFun[(A, B), C] =
        Uncurry(A.build(a => B.build(b => f((a, b)))))
    }

  implicit def boolean: PartialFunArg[Boolean] = {
    type R = Either[Unit, Unit]
    val R: PartialFunArg[R] = either(unit, unit)

    def embed(x: Boolean): R = if (x) Right(()) else Left(())
    def eject(y: R): Boolean = y.fold(_ => false, _ => true)

    R.by(embed, eject)
  }

  implicit def list[A](implicit A: PartialFunArg[A]): PartialFunArg[List[A]] = new PartialFunArg[List[A]] { listA =>
    type R = Either[Unit, (A, List[A])]
    implicit val R: PartialFunArg[R] = either(unit, pair(A, listA))

    def embed(x: List[A]): R = x match {
      case Nil     => Left(())
      case a :: as => Right((a, as))
    }
    def eject(y: R): List[A] = y match {
      case Left(_)        => Nil
      case Right((a, as)) => a :: as
    }

    def build[B](f: List[A] => B): PartialFun[List[A], B] =
      Iso.build(embed, eject, f)
  }

  implicit val int: PartialFunArg[Int] = new PartialFunArg[Int] { int =>
    type R = Either[(Boolean, Int), Boolean]
    implicit val R: PartialFunArg[R] = either(pair(boolean, int), boolean)

    def embed(x: Int): R = x match {
      case 0  => Right(false)
      case -1 => Right(true)
      case _  => Left((x % 2 != 0, x / 2))
    }
    def eject(y: R): Int = y match {
      case Right(false)     => 0
      case Right(true)      => -1
      case Left((true, x))  => 2 * x + 1
      case Left((false, x)) => 2 * x
    }

    def build[B](f: Int => B): PartialFun[Int, B] =
      Iso.build(embed, eject, f)
  }

  implicit val char: PartialFunArg[Char] = int.by(_.toInt, _.toChar)
  implicit def array[A: PartialFunArg: ClassTag]: PartialFunArg[Array[A]] = list[A].by(_.toList, _.toArray)
  implicit val string: PartialFunArg[String] = array[Char].by(_.toCharArray, String.valueOf(_))
}

final case class Fun[A, B](ab: PartialFun[A, B], d: B, isShrunk: Boolean, f: A => B) {
  def apply(a: A): B = f(a)

  override def toString: String =
    if (isShrunk) {
      val abs = ab.table.map { case (a, b) => s"$a -> $b" }.mkString(", ")
      s"Fun.from($abs)($d)"
    } else s"Fun($ab, $d, $isShrunk, $f)"
}

object Fun {
  def from[A, B](abs: (A, B)*)(d: B): Fun[A, B] = ???

  def gen[A: Cogen: PartialFunArg, B: Gen](isShrunk: Boolean): Gen[Fun[A, B]] =
    Gen[(A => B, B)].map { case (f, d) => Fun(PartialFun(f), d, isShrunk, f) }
  @silent def shrink[A, B](isShrunk: Boolean)(implicit s: Shrink[B]): Shrink[Fun[A, B]] =
    Shrink.shrink {
      case fun @ Fun(ab, d, shrunk, f) =>
        ab.shrink(b => LazyList.from(s(b))).map(ab1 => Fun(ab1, d, isShrunk, ab1.toFunction(d))).toStream #:::
          s(d).map(d1 => new Fun(ab, d1, isShrunk, ab.toFunction(d1))) #:::
          (if (shrunk) Stream.empty else Stream(fun.copy(isShrunk = true)))
    }

  implicit def scalapropsGenInstance[A: Cogen: PartialFunArg, B: Gen]: Gen[Fun[A, B]] = gen(false)
  implicit def scalapropsShrinkInstance[A, B](implicit s: Shrink[B]): Shrink[Fun[A, B]] = shrink(false)
}

package object fun {
  @silent implicit val unitShrinkInstance: Shrink[Unit] =
    Shrink.shrink(_ => Stream.empty)

  @silent implicit val intShrinkInstance: Shrink[Int] =
    Shrink.shrink {
      case 0 => Stream.empty
      case i =>
        val is = Stream.iterate(i)(_ / 2).takeWhile(_ != 0).map(i - _)
        if (i < 0 && i != -i) -i #:: is else is
    }

  @silent implicit val booleanShrinkInstance: Shrink[Boolean] =
    Shrink.shrink {
      case true  => Stream(false)
      case flase => Stream.empty
    }
}
