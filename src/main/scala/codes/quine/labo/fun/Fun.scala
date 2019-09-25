// This file provides lazy shrinkable function representation for Scalaprops.
//
// In other words, it is port of Haskell's Test.QuickCheck.Function in Scala.
// http://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck-Function.html
//
// Working example can be seen at test/scala/codes/quine/labo/fun/FunProps.scala

package codes.quine.labo
package fun

import com.github.ghik.silencer.silent
import java.nio.ByteBuffer
import scalaprops.{Cogen, Gen, Shrink}
import scala.reflect.ClassTag
import scala.util.chaining._
import simulacrum.typeclass

final class Lazy[A] private (private[this] var thunk: () => A) {
  lazy val value = {
    val a = thunk()
    thunk = null
    a
  }

  override def toString: String =
    if (thunk == null) value.toString else "<not computed>"
}

object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy(value _)
}

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
  def lift[A, B](domain: Set[A], f: A => B): PartialFun[A, B] =
    if (domain.isEmpty) Empty[A, B] else Lift(domain, f)

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

  final class Point[B] private (b: B) extends PartialFun[Unit, B] {
    def isEmpty: Boolean = false
    def map[C](f: B => C): PartialFun[Unit, C] = Point(f(b))
    def table: LazyList[(Unit, B)] = LazyList.cons(() -> b, LazyList.empty)
    def toFunction(d: B): Unit => B = _ => b
    def shrink(s: B => LazyList[B]): LazyList[PartialFun[Unit, B]] = s(b).map(point(_))

    override def toString: String = s"Point($b.value)"
  }

  object Point {
    def apply[B](b: B): Point[B] = new Point(b)
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

  final class Iso[A, B, C] private (val embed: A => B, val eject: B => A, bc: PartialFun[B, C])
      extends PartialFun[A, C] {
    def isEmpty: Boolean = false
    def map[D](f: C => D): PartialFun[A, D] = Iso(embed, eject, bc.map(f))
    def table: LazyList[(A, C)] =
      bc.table.map { case (b, c) => (eject(b), c) }
    def toFunction(d: C): A => C = {
      lazy val g = bc.toFunction(d)
      (a: A) => g(embed(a))
    }
    def shrink(s: C => LazyList[C]): LazyList[PartialFun[A, C]] =
      bc.shrink(s).map(iso(embed, eject, _))

    override def toString: String = s"Iso($embed, $eject, $bc)"
  }

  object Iso {
    def apply[A, B, C](embed: A => B, eject: B => A, bc: PartialFun[B, C]): Iso[A, B, C] =
      new Iso(embed, eject, bc)

    def build[A, B, C](embed: A => B, eject: B => A, f: A => C)(implicit B: PartialFunArg[B]): Iso[A, B, C] =
      Iso(embed, eject, B.build(b => f(eject(b))))
  }

  final class Lift[A, B] private (val domain: Set[A], val f: A => B) extends PartialFun[A, B] {
    def isEmpty: Boolean = false
    def map[C](g: B => C): PartialFun[A, C] = Lift(domain, f.andThen(g))
    def table: LazyList[(A, B)] = LazyList.from(domain).map(a => (a, f(a)))
    def toFunction(d: B): A => B =
      (a: A) => if (domain.contains(a)) f(a) else d
    def shrink(s: B => LazyList[B]): LazyList[PartialFun[A, B]] = {
      val dom = domain.toList
      LazyList.from(Shrink.list(Shrink.empty)(dom).map(dom1 => lift(dom1.toSet, f))) #:::
        LazyList.from(dom).map(a => (a, f(a))).flatMap {
          case (a, b) => s(b).map(b1 => lift(domain, Map(a -> b1).withDefault(f)))
        }
    }

    override def toString: String = s"Lift($domain, $f)"
  }

  object Lift {
    def apply[A, B](domain: Set[A], f: A => B): Lift[A, B] = new Lift(domain, f)
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

  implicit def tuple2[A, B](implicit A: PartialFunArg[A], B: PartialFunArg[B]): PartialFunArg[(A, B)] =
    new PartialFunArg[(A, B)] {
      def build[C](f: ((A, B)) => C): PartialFun[(A, B), C] =
        Uncurry(A.build(a => B.build(b => f((a, b)))))
    }
  implicit def tuple3[A: PartialFunArg, B: PartialFunArg, C: PartialFunArg]: PartialFunArg[(A, B, C)] =
    PartialFunArg[(A, (B, C))].by({ case (a, b, c) => (a, (b, c)) }, { case (a, (b, c)) => (a, b, c) })
  implicit def tuple4[A: PartialFunArg, B: PartialFunArg, C: PartialFunArg, D: PartialFunArg]
    : PartialFunArg[(A, B, C, D)] =
    PartialFunArg[(A, B, (C, D))].by({ case (a, b, c, d) => (a, b, (c, d)) }, { case (a, b, (c, d)) => (a, b, c, d) })

  implicit def boolean: PartialFunArg[Boolean] = {
    type R = Either[Unit, Unit]
    val R: PartialFunArg[R] = either(unit, unit)

    def embed(x: Boolean): R = if (x) Right(()) else Left(())
    def eject(y: R): Boolean = y.fold(_ => false, _ => true)

    R.by(embed, eject)
  }

  implicit def list[A](implicit A: PartialFunArg[A]): PartialFunArg[List[A]] = new PartialFunArg[List[A]] { listA =>
    type R = Either[Unit, (A, List[A])]
    implicit val R: PartialFunArg[R] = either(unit, tuple2(A, listA))

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

  implicit val byte: PartialFunArg[Byte] = new PartialFunArg[Byte] { b =>
    val domain: Set[Byte] = (-128 to 127).map(_.asInstanceOf[Byte]).toSet
    def build[B](f: Byte => B): PartialFun[Byte, B] =
      Lift(domain, f)
  }

  implicit val int: PartialFunArg[Int] = {
    type R = (Byte, Byte, Byte, Byte)

    def embed(x: Int): R =
      ByteBuffer.allocate(4).putInt(x).array().pipe(bb => (bb(0), bb(1), bb(2), bb(3)))

    def eject(y: R): Int =
      ByteBuffer.wrap(Array(y._1, y._2, y._3, y._4)).getInt()

    PartialFunArg[R].by(embed, eject)
  }

  implicit val char: PartialFunArg[Char] = int.by(_.toInt, _.toChar)
  implicit def array[A: PartialFunArg: ClassTag]: PartialFunArg[Array[A]] = list[A].by(_.toList, _.toArray)
  implicit val string: PartialFunArg[String] = array[Char].by(_.toCharArray, String.valueOf(_))
  implicit val bigInt: PartialFunArg[BigInt] = array[Byte].by(_.toByteArray, x => if (x.length == 0) 0 else BigInt(x))
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
  def from[A, B](abs: (A, B)*)(d: B): Fun[A, B] = {
    val f = abs.toMap
    val ab = PartialFun.lift(f.keySet, f)
    Fun(ab, d, true, f.withDefault(_ => d))
  }

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
