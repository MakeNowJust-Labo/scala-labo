package codes.quine.labo.nyan
package data

final case class Id[A](run: A)

object Id {
  implicit object IdInstances extends Comonad[Id] with Monad[Id] {
    def pure[A](a: A): Id[A] = Id(a)

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa.run)

    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = Id(f(fa.run))

    def extract[A](fa: Id[A]): A = fa.run

    def coflatMap[A, B](fa: Id[A])(f: Id[A] => B): Id[B] = Id(f(fa))
  }
}