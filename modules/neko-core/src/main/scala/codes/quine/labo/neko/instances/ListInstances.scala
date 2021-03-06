package codes.quine.labo
package neko
package instances

import scala.annotation.tailrec
import data._, syntax._

trait ListInstances extends ListInstances0

private[instances] trait ListInstances0 extends ListInstances1 {
  implicit val listMonadInstance: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
      val buf = List.newBuilder[B]
      @tailrec
      def go(stack: List[List[Either[A, B]]]): Unit =
        stack match {
          case Nil         => ()
          case Nil :: tail => go(tail)
          case (ab :: abs) :: tail =>
            ab match {
              case Right(b) =>
                buf += b
                go(abs :: tail)
              case Left(a) =>
                go(f(a) :: abs :: tail)
            }
        }
      go(f(a) :: Nil)
      buf.result
    }

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit def listEqInstance[A: Eq]: Eq[List[A]] = new Eq[List[A]] {
    @tailrec
    def eqv(x: List[A], y: List[A]): Boolean =
      (x, y) match {
        case (Nil, Nil)                        => true
        case (x0 :: xs, y0 :: ys) if x0 === y0 => eqv(xs, ys)
        case _                                 => false
      }
  }

  implicit def listPartialOrdInstance[A: PartialOrd]: PartialOrd[List[A]] = new PartialOrd[List[A]] {
    @tailrec
    def tryCmp(x: List[A], y: List[A]): Option[Ordering] =
      (x, y) match {
        case (Nil, Nil) => Some(Ordering.EQ)
        case (Nil, _)   => Some(Ordering.LT)
        case (_, Nil)   => Some(Ordering.GT)
        case (x0 :: xs, y0 :: ys) =>
          PartialOrd[A].tryCmp(x0, y0) match {
            case Some(Ordering.EQ) => tryCmp(xs, ys)
            case result            => result
          }
      }
  }

  implicit def listOrdInstance[A: Ord]: Ord[List[A]] = new Ord[List[A]] {
    @tailrec
    def cmp(x: List[A], y: List[A]): Ordering =
      (x, y) match {
        case (Nil, Nil) => Ordering.EQ
        case (Nil, _)   => Ordering.LT
        case (_, Nil)   => Ordering.GT
        case (x0 :: xs, y0 :: ys) =>
          (x0 <=> y0) match {
            case Ordering.EQ => cmp(xs, ys)
            case result      => result
          }
      }
  }
}

private[instances] trait ListInstances1 { self: ListInstances0 =>
  implicit def listMonoidInstance[A]: Monoid[List[A]] = listAlternativeInstance.algebra[A]

  implicit val listAlternativeInstance: Alternative[List] = new Alternative[List] {
    def pure[A](a: A): List[A] = List(a)
    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = ff.flatMap(f => fa.map(f))
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    def emptyK[A]: List[A] = List.empty
    def concatK[A](x: List[A], y: List[A]): List[A] = x ++ y
  }

  implicit val listCoflatMapInstance: CoflatMap[List] = new CoflatMap[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    def coflatMap[A, B](fa: List[A])(f: List[A] => B): List[B] = fa.tails.toList.init.map(f)
  }

  implicit val listTraverseInstance: Traverse[List] = new Traverse[List] {
    override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    override def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(fa: List[A]): Eval[B] = fa match {
        case Nil     => lb
        case x :: xs => f(x, Eval.defer(loop(xs)))
      }
      Eval.defer(loop(fa))
    }

    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      foldRight(fa, Eval.always(Applicative[G].pure(List.empty[B]))) { (a, lgb) =>
        lgb.map(gb => Applicative[G].map2(f(a), gb)(_ :: _))
      }.value
  }

  implicit def listHashInstance[A: Hash]: Hash[List[A]] = new Hash[List[A]] {
    def eqv(x: List[A], y: List[A]): Boolean = self.listEqInstance[A].eqv(x, y)
    def hash(x: List[A]): Int = x.foldLeft("List".hash)(_ * 31 + _.hash)
  }
}

package object list extends ListInstances
