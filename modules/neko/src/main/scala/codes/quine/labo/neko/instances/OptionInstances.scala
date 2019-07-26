package codes.quine.labo.neko
package instances

import scala.annotation.tailrec

trait OptionInstances {
  implicit object OptionInstances extends Alternative[Option] with Monad[Option] {
    def pure[A](a: A): Option[A] = Some(a)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(x))  => tailRecM(x)(f)
        case Some(Right(y)) => Some(y)
      }

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    def empty[A]: Option[A] = None

    def combine[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
  }
}
