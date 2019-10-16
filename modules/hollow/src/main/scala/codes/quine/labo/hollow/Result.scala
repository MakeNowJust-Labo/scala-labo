package codes.quine.labo
package hollow

import neko.{Applicative, Traverse}

import Result._

sealed trait Result[T] { self =>
  def map[U](f: T => U): Result[U] = self match {
    case Success(value, info) => Success(f(value), info)
    case Failure(error)       => Failure(error)
  }

  def traverse[F[_]: Applicative, U](f: T => F[U]): F[Result[U]] = self match {
    case Success(value, info) => Applicative[F].map(f(value))(Success(_, info))
    case Failure(error)       => Applicative[F].pure(Failure(error))
  }
}

object Result {
  final case class Success[T](value: T, input: ParseInput) extends Result[T]
  final case class Failure[T](error: ParseError) extends Result[T]
}

trait ResultInstances {
  implicit val resultTraverseInstance: Traverse[Result] = new Traverse[Result] {
    override def map[A, B](fa: Result[A])(f: A => B): Result[B] = fa.map(f)
    def traverse[G[_]: Applicative, A, B](fa: Result[A])(f: A => G[B]): G[Result[B]] = fa.traverse(f)
  }
}
