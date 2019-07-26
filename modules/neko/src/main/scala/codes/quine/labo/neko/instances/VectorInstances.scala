package codes.quine.labo.neko
package instances

import scala.annotation.tailrec

import syntax._

trait VectorInstances {
  implicit def VectorEq[A: Eq]: Eq[Vector[A]] = new Eq[Vector[A]] {
    @tailrec
    def eqv(x: Vector[A], y: Vector[A]): Boolean =
      (x, y) match {
        case (Vector(), Vector())          => true
        case (a +: xs, b +: ys) if a === b => eqv(xs, ys)
        case _                             => false
      }
  }

  implicit object VectorInstances extends Alternative[Vector] with Monad[Vector] {
    def pure[A](a: A): Vector[A] = Vector(a)

    def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] =
      fa.flatMap(f)

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
                go(tail)
              case Left(a) => go(f(a) :: abs :: tail)
            }
        }
      go(f(a) :: Nil)
      buf.result
    }

    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)

    def empty[A]: Vector[A] = Vector.empty[A]

    def combine[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
  }
}
