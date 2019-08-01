package codes.quine.labo
package neko
package data

import scala.annotation.tailrec

final case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  implicit def IdEqInstances[A: Eq]: Eq[Id[A]] = Eq[A].by(_.value)

  implicit object IdInstances extends Comonad[Id] with Monad[Id] {
    def pure[A](a: A): Id[A] = Id(a)

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)

    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = fa.map(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(a) match {
      case Id(Left(x))  => tailRecM(x)(f)
      case Id(Right(y)) => Id(y)
    }

    def extract[A](fa: Id[A]): A = fa.value

    def coflatMap[A, B](fa: Id[A])(f: Id[A] => B): Id[B] = Id(f(fa))
  }
}
