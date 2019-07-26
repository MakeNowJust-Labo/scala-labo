package codes.quine.labo.neko
package instances

import scala.annotation.tailrec

import syntax._

trait ListInstances {
  implicit def ListEq[A: Eq]: Eq[List[A]] = new Eq[List[A]] {
    @tailrec
    def eqv(x: List[A], y: List[A]): Boolean =
      (x, y) match {
        case (Nil, Nil)                    => true
        case (a :: xs, b :: ys) if a === b => eqv(xs, ys)
        case _                             => false
      }
  }

  implicit object ListInstances extends Alternative[List] with Monad[List] {
    def pure[A](a: A): List[A] = List(a)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
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

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = {
      val buf = List.newBuilder[B]
      @tailrec
      def go(funs: List[A => B]): Unit = funs match {
        case f :: fs =>
          buf ++= fa.map(f)
          go(fs)
        case Nil => ()
      }
      go(ff)
      buf.result
    }

    def empty[A]: List[A] = List.empty

    def combine[A](x: List[A], y: List[A]): List[A] = x ++ y
  }
}
