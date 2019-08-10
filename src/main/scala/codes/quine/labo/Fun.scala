package codes.quine.labo

import scala.math.BigInt
import scala.reflect.ClassTag
import scalaprops.{Cogen, Gen, Shrink}
import simulacrum.typeclass

sealed trait :->[A, B] {
  type I = A
  type O = B
  def map[C](f: B => C): A :-> C = :->.map(this)(f)
  def table: LazyList[(A, B)] = :->.table(this)
  def toFunction(d: B): A => B = :->.toFunction(this, d)
}

object :-> {
  final case class FUnit[A](a: A) extends :->[Unit, A]
  final case class FNil[A, B]() extends :->[A, B]
  final case class FPair[A, B, C](abc: A :-> (B :-> C)) extends :->[(A, B), C]
  final case class FSelect[A, B, C](ac: A :-> C, bc: () => B :-> C) extends :->[Either[A, B], C]
  final case class FTable[A, B](abs: Stream[(A, B)]) extends :->[A, B]
  final case class FMap[A, B, C](f: A => B, g: B => A, bc: B :-> C) extends :->[A, C]

  private[labo] def map[A, B, C](ab: A :-> B)(f: B => C): A :-> C =
    ab match {
      case FUnit(a)         => FUnit(f(a))
      case _: FNil[A, B]    => FNil()
      case FPair(abc)       => FPair(map(abc)(map(_)(f)))
      case FSelect(ac, bc)  => FSelect(map(ac)(f), () => map(bc())(f))
      case FTable(abs)      => FTable(abs.map { case (a, b) => (a, f(b)) })
      case FMap(f1, g1, bc) => FMap(f1, g1, map(bc)(f))
    }

  private[labo] def table[A, B](ab: A :-> B): LazyList[(A, B)] =
    ab match {
      case FUnit(a)      => LazyList(((), a))
      case _: FNil[A, B] => LazyList()
      case FPair(abc) =>
        table(abc).flatMap { case (a, bc) => table(bc).map { case (b, c) => ((a, b), c) } }
      case FSelect(ac, bc) =>
        table(ac).map { case (a, c) => (Left(a): A, c) } ++ table(bc()).map { case (b, c) => (Right(b), c) }
      case FTable(abs) => LazyList.from(abs)
      case FMap(_, g, bc) =>
        table(bc).map { case (b, c) => (g(b), c) }
    }

  private[labo] def toFunction[A, B](ab: A :-> B, d: B): A => B =
    ab match {
      case FUnit(a)        => _ => a
      case _: FNil[A, B]   => _ => d
      case FPair(abc)      => { case (a, b) => toFunction(abc.map(bc => toFunction(bc, d)(b)), d)(a) }
      case FSelect(ac, bc) => e => e.fold(toFunction(ac, d), b => toFunction(bc(), d)(b))
      case FTable(abs)     => a => abs.filter(_._1 == a).headOption.map(_._2).getOrElse(d)
      case FMap(f, _, bc)  => a => toFunction(bc, d)(f(a))
    }

  private[labo] def unlessNil[A0, B0, A, B](ab0: A0 :-> B0, ab: => A :-> B): Stream[A :-> B] = ab0 match {
    case _: FNil[A0, B0] => Stream.empty
    case _               => Stream(ab)
  }

  private[labo] def fpair[A, B, C](ab: A :-> (B :-> C)): (A, B) :-> C = ab match {
    case _: FNil[A, B :-> C] => FNil[(A, B), C]
    case abc                 => FPair(abc)
  }

  private[labo] def fselect[A, B, C](ac: A :-> C, bc: B :-> C): Either[A, B] :-> C = (ac, bc) match {
    case (_: FNil[A, C], _: FNil[B, C]) => FNil[Either[A, B], C]
    case _                              => FSelect(ac, () => bc)
  }

  private[labo] def fmap[A, B, C](f: A => B, g: B => A, bc: B :-> C): A :-> C = bc match {
    case _: FNil[B, C] => FNil[A, C]
    case _             => FMap(f, g, bc)
  }

  private[labo] def ftable[A, B](abs: Stream[(A, B)]): A :-> B =
    if (abs.isEmpty) FNil[A, B] else FTable(abs)

  def shrink[A, B](ab: A :-> B)(shr: B => Stream[B]): Stream[A :-> B] = ab match {
    case FUnit(a)      => shr(a).map(FUnit(_))
    case _: FNil[A, B] => Stream.empty
    case FPair(abc) =>
      shrink(abc)(bc => shrink(bc)(shr)).map(fpair(_))
    case FSelect(ac, bc0) =>
      val bc = bc0()
      unlessNil(bc, fselect(ac, FNil[bc.I, bc.O])) #:::
        unlessNil(ac, fselect(FNil[ac.I, ac.O], bc)) #:::
        shrink(bc)(shr).map(fselect(_, bc)) #:::
        shrink(ac)(shr).map(fselect(ac, _))
    case FTable(abs) =>
      Shrink.stream(Shrink.shrink[(A, B)] { case (a, b) => shr(b).map((a, _)) })(abs).map(ftable(_))
    case FMap(f, g, bc) =>
      shrink(bc)(shr).map(fmap(f, g, _))
  }

  implicit def squidShrinkInstance[A, B: Shrink]: Shrink[A :-> B] = Shrink.shrink(shrink(_)(Shrink[B].apply))

  implicit def squidGenInstance[A: Arg: Cogen, B: Gen]: Gen[A :-> B] = Gen[A => B].map(Arg[A].function(_))
}

@typeclass trait Arg[A] { self =>
  import :->._

  def function[B](f: A => B): A :-> B

  def imap[B](to: B => A, from: A => B): Arg[B] = new Arg[B] {
    def function[C](f: B => C): B :-> C =
      FMap(to, from, self.function(a => f(from(a))))
  }
}

object Arg {
  import :->._

  implicit val unitArgInstance: Arg[Unit] = new Arg[Unit] {
    def function[B](f: Unit => B): Unit :-> B = FUnit(f(()))
  }

  implicit def eitherArgInstance[A: Arg, B: Arg]: Arg[Either[A, B]] = new Arg[Either[A, B]] {
    def function[C](f: Either[A, B] => C): Either[A, B] :-> C =
      FSelect(Arg[A].function(a => f(Left(a))), () => Arg[B].function(b => f(Right(b))))
  }

  implicit def pairArgInstance[A: Arg, B: Arg]: Arg[(A, B)] = new Arg[(A, B)] {
    def function[C](f: ((A, B)) => C): (A, B) :-> C =
      FPair(Arg[A].function(a => (b: B) => f((a, b))).map(Arg[B].function))
  }

  implicit def listArgInstance[A: Arg]: Arg[List[A]] = new Arg[List[A]] {
    type Repr = Either[Unit, (A, List[A])]
    val Repr: Arg[Repr] = eitherArgInstance(unitArgInstance, pairArgInstance(Arg[A], this))

    def to(l: List[A]): Repr = l match {
      case Nil     => Left(())
      case x :: xs => Right((x, xs))
    }

    def from(e: Repr): List[A] = e match {
      case Left(_)        => Nil
      case Right((x, xs)) => x :: xs
    }

    def function[B](f: List[A] => B): List[A] :-> B =
      FMap(to, from, Repr.function(e => f(from(e))))
  }

  implicit val byteArgInstance: Arg[Byte] = new Arg[Byte] {
    def function[B](f: Byte => B): Byte :-> B =
      FTable((-128 to 127).map(i => (i.toByte, f(i.toByte))).toStream)
  }

  implicit val bigIntArgInstance: Arg[BigInt] = new Arg[BigInt] {
    type Nat = List[Byte]
    val Nat: Arg[Nat] = listArgInstance(byteArgInstance)
    type Repr = Either[Nat, Nat]
    val Repr: Arg[Repr] = eitherArgInstance(Nat, Nat)

    def to(i: BigInt): Repr =
      if (i < 0) Right(toNat(i.abs - 1)) else Left(toNat(i))

    def from(e: Repr): BigInt = e match {
      case Left(l)  => fromNat(l)
      case Right(l) => -(fromNat(l) + 1)
    }

    def toNat(i: BigInt): Nat =
      if (i == 0) Nil
      else {
        val (d, r) = i /% 256
        (r - 128).toByte :: toNat(d)
      }

    def fromNat(l: Nat): BigInt = l match {
      case Nil     => 0
      case x :: xs => (x + 128) + fromNat(xs) * 256
    }

    def function[B](f: BigInt => B): BigInt :-> B =
      FMap(to, from, Repr.function(e => f(from(e))))
  }

  implicit val booleanArgInstance: Arg[Boolean] =
    Arg[Either[Unit, Unit]].imap(if (_) Right(()) else Left(()), _.fold(_ => false, _ => true))
  implicit val intArgInstance: Arg[Int] = Arg[BigInt].imap(BigInt(_), _.toInt)
  implicit val charArgInstance: Arg[Char] = Arg[BigInt].imap(c => BigInt(c.toInt), _.toChar)
  implicit def arrayArgInstance[A: Arg: ClassTag]: Arg[Array[A]] = Arg[List[A]].imap(_.toList, _.toArray)
  implicit val stringArgInstance: Arg[String] = Arg[Array[Char]].imap(_.toCharArray, String.valueOf(_))
}

final case class Fun[A, B](ab: A :-> B, d: B, shrunk: Boolean, f: A => B) {
  override def toString: String =
    if (shrunk) {
      val abs = ab.table
      val b1 = abs.map { case (a, b) => s"case $a => $b" }.mkString(", ")
      val b2 = s"case _ => $d"
      s"{ $b1, $b2 }"
    } else "<fun>"

  def apply(a: A): B = f(a)
}

object Fun {
  import :->._

  implicit def funGenInstance[A: Cogen: Arg, B: Gen]: Gen[Fun[A, B]] = Gen[(A => B, B)].map {
    case (f, d) => Fun(Arg[A].function(f), d, false, f)
  }
  implicit def funShrinkInstance[A, B: Shrink]: Shrink[Fun[A, B]] = Shrink.shrink {
    case Fun(ab, d, shrunk, f) =>
      val abds = Shrink[(A :-> B, B)].apply((ab, d))
      abds.map { case (ab, d) => Fun(ab, d, false, ab.toFunction(d)) } ++ (if (shrunk) Stream.empty
                                                                           else Stream(Fun(ab, d, true, f)))
  }
}
