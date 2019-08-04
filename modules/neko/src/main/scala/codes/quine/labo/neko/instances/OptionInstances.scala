package codes.quine.labo
package neko
package instances

import scala.annotation.tailrec
import data._, syntax._

trait OptionInstances extends OptionInstances0

private[instances] trait OptionInstances0 extends OptionInstances1 {
  implicit val optionMonadInstance: Monad[Option] = new Monad[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case Some(Right(b)) => Some(b)
        case Some(Left(a0)) => tailRecM(a0)(f)
        case None           => None
      }
  }

  implicit def optionEqInstance[A: Eq]: Eq[Option[A]] = new Eq[Option[A]] {
    def eqv(x: Option[A], y: Option[A]): Boolean =
      (x, y) match {
        case (None, None)                      => true
        case (Some(x0), Some(y0)) if x0 === y0 => true
        case _                                 => false
      }
  }

  implicit def optionPartialOrdInstance[A: PartialOrd]: PartialOrd[Option[A]] = new PartialOrd[Option[A]] {
    def tryCmp(x: Option[A], y: Option[A]): Option[Ordering] =
      (x, y) match {
        case (None, None)         => Some(Ordering.EQ)
        case (None, _)            => Some(Ordering.LT)
        case (_, None)            => Some(Ordering.GT)
        case (Some(x0), Some(y0)) => PartialOrd[A].tryCmp(x0, y0)
      }
  }

  implicit def optionOrdInstance[A: Ord]: Ord[Option[A]] = new Ord[Option[A]] {
    def cmp(x: Option[A], y: Option[A]): Ordering =
      (x, y) match {
        case (None, None)         => Ordering.EQ
        case (None, _)            => Ordering.LT
        case (_, None)            => Ordering.GT
        case (Some(x0), Some(y0)) => Ord[A].cmp(x0, y0)
      }
  }
}

private[instances] trait OptionInstances1 { self: OptionInstances0 =>
  implicit val optionAlternativeInstance: Alternative[Option] = new Alternative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      ff match {
        case Some(f) => fa.map(f)
        case None    => None
      }

    def emptyK[A]: Option[A] = None

    def concatK[A](x: Option[A], y: Option[A]): Option[A] =
      x match {
        case None => y
        case _    => x
      }
  }

  implicit def optionHashInstance[A: Hash]: Hash[Option[A]] = new Hash[Option[A]] {
    def eqv(x: Option[A], y: Option[A]): Boolean = self.optionEqInstance[A].eqv(x, y)
    def hash(x: Option[A]): Int = x match {
      case Some(a) => "Some".hash * 31 + a.hash
      case None    => "None".hash
    }
  }
}

package object option extends OptionInstances
