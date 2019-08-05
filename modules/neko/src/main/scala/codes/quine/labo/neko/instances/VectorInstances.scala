package codes.quine.labo
package neko
package instances

import scala.annotation.tailrec
import data._, syntax._

trait VectorInstances extends VectorInstances0

private[instances] trait VectorInstances0 extends VectorInstances1 {
  implicit val vectorMonadInstance: Monad[Vector] = new Monad[Vector] {
    def pure[A](a: A): Vector[A] = Vector(a)
    override def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Vector[Either[A, B]]): Vector[B] = {
      val buf = Vector.newBuilder[B]
      @tailrec
      def go(stack: List[Vector[Either[A, B]]]): Unit =
        stack match {
          case Nil              => ()
          case Vector() :: tail => go(tail)
          case (ab +: abs) :: tail =>
            ab match {
              case Right(b) =>
                buf += b
                go(abs :: tail)
              case Left(a) => go(f(a) :: abs :: tail)
            }
        }
      go(f(a) :: Nil)
      buf.result
    }

    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
  }

  implicit def vectorEqInstance[A: Eq]: Eq[Vector[A]] = new Eq[Vector[A]] {
    @tailrec
    def eqv(x: Vector[A], y: Vector[A]): Boolean =
      (x, y) match {
        case (Vector(), Vector())              => true
        case (x0 +: xs, y0 +: ys) if x0 === y0 => eqv(xs, ys)
        case _                                 => false
      }
  }

  implicit def vectorPartialOrdInstance[A: PartialOrd]: PartialOrd[Vector[A]] = new PartialOrd[Vector[A]] {
    @tailrec
    def tryCmp(x: Vector[A], y: Vector[A]): Option[Ordering] =
      (x, y) match {
        case (Vector(), Vector()) => Some(Ordering.EQ)
        case (Vector(), _)        => Some(Ordering.LT)
        case (_, Vector())        => Some(Ordering.GT)
        case (x0 +: xs, y0 +: ys) =>
          PartialOrd[A].tryCmp(x0, y0) match {
            case Some(Ordering.EQ) => tryCmp(xs, ys)
            case result            => result
          }
      }
  }

  implicit def vectorOrdInstance[A: Ord]: Ord[Vector[A]] = new Ord[Vector[A]] {
    @tailrec
    def cmp(x: Vector[A], y: Vector[A]): Ordering =
      (x, y) match {
        case (Vector(), Vector()) => Ordering.EQ
        case (Vector(), _)        => Ordering.LT
        case (_, Vector())        => Ordering.GT
        case (x0 +: xs, y0 +: ys) =>
          (x0 <=> y0) match {
            case Ordering.EQ => cmp(xs, ys)
            case result      => result
          }
      }
  }
}

private[instances] trait VectorInstances1 { self: VectorInstances0 =>
  implicit def vectorMonoidInstance[A]: Monoid[Vector[A]] = vectorAlternativeInstance.algebra[A]

  implicit val vectorAlternativeInstance: Alternative[Vector] = new Alternative[Vector] {
    def pure[A](a: A): Vector[A] = Vector(a)
    def ap[A, B](ff: Vector[A => B])(fa: Vector[A]): Vector[B] = ff.flatMap(f => fa.map(f))
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
    def emptyK[A]: Vector[A] = Vector.empty
    def concatK[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
  }

  implicit val vectorCoflatMapInstance: CoflatMap[Vector] = new CoflatMap[Vector] {
    def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
    def coflatMap[A, B](fa: Vector[A])(f: Vector[A] => B): Vector[B] = fa.tails.toVector.init.map(f)
  }

  implicit def vectorHashInstance[A: Hash]: Hash[Vector[A]] = new Hash[Vector[A]] {
    def eqv(x: Vector[A], y: Vector[A]): Boolean = self.vectorEqInstance[A].eqv(x, y)
    def hash(x: Vector[A]): Int = x.foldLeft("Vector".hash)(_ * 31 + _.hash)
  }
}

package object vector extends VectorInstances
