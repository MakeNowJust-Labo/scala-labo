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

  implicit def optionEqInstance[A: Eq]: Eq[Option[A]] = Eq.eqv {
    case (None, None)                      => true
    case (Some(x0), Some(y0)) if x0 === y0 => true
    case _                                 => false
  }

  implicit def optionPartialOrdInstance[A: PartialOrd]: PartialOrd[Option[A]] = PartialOrd.tryCmp {
    case (None, None)         => Some(Ordering.EQ)
    case (None, _)            => Some(Ordering.LT)
    case (_, None)            => Some(Ordering.GT)
    case (Some(x0), Some(y0)) => x0.tryCmp(y0)
  }

  implicit def optionOrdInstance[A: Ord]: Ord[Option[A]] = Ord.cmp {
    case (None, None)         => Ordering.EQ
    case (None, _)            => Ordering.LT
    case (_, None)            => Ordering.GT
    case (Some(x0), Some(y0)) => x0 <=> y0
  }

  implicit def optionMonoidInstance[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def empty: Option[A] = None

    def concat(x: Option[A], y: Option[A]): Option[A] = (x, y) match {
      case (Some(a1), Some(a2)) => Some(a1 |+| a2)
      case (None, Some(a))      => Some(a)
      case (Some(a), None)      => Some(a)
      case (None, None)         => None
    }
  }
}

private[instances] trait OptionInstances1 { self: OptionInstances0 =>
  implicit val optionAlternativeInstance: Alternative[Option] = new Alternative[Option] {
    def pure[A](a: A): Option[A] = Some(a)
    def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      ff.flatMap(f => fa.map(f))

    def emptyK[A]: Option[A] = None
    def concatK[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
  }

  implicit val optionTraverseInstance: Traverse[Option] = new Traverse[Option] {
    def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
      case None    => Applicative[G].pure(None)
      case Some(a) => f(a).map(Some(_))
    }
    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
      case None    => b
      case Some(a) => f(b, a)
    }
    override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case None    => lb
      case Some(a) => Eval.defer(f(a, lb))
    }
  }

  implicit val optionCoflatMapInstance: CoflatMap[Option] = new CoflatMap[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    def coflatMap[A, B](fa: Option[A])(f: Option[A] => B): Option[B] = fa match {
      case None    => None
      case Some(_) => Some(f(fa))
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
