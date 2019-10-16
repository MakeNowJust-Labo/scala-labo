package codes.quine.labo
package hollow

import neko.data.Eval
import neko.syntax._

import Result._, ParseError._

trait Parser[T] { p =>
  def apply[R](
    onSuccess0: T => Eval[R],
    onFailure0: ParseError => Eval[R],
    onSuccess: (T, ParseInput) => Eval[R],
    onFailure: ParseError => Eval[R],
    input: ParseInput
  ): Eval[R]

  def run(input: ParseInput): Result[T] = {
    def success0(t: T): Eval[Result[T]] = Eval.now(Success(t, input))
    def success(t: T, in: ParseInput): Eval[Result[T]] = Eval.now(Success(t, in))
    def failure(err: ParseError): Eval[Result[T]] = Eval.now(Failure(err))

    apply(success0, failure, success, failure, input).value
  }

  def choice[U](q: Parser[U]): Parser[Either[T, U]] = new Parser[Either[T, U]] {
    def apply[R](onSuccess0: Either[T, U] => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (Either[T, U], ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] = {
      def failure0(err: ParseError): Eval[R] =
        Eval.defer(q.apply(success00, failure00(err), success01, failure01(err), input))
      def failure00(e1: ParseError)(e2: ParseError): Eval[R] = onFailure0(e1.merge(e2))
      def failure01(e1: ParseError)(e2: ParseError): Eval[R] = onFailure(e1.merge(e2))
      def success0(t: T): Eval[R] = onSuccess0(Left(t))
      def success1(t: T, in: ParseInput): Eval[R] = onSuccess(Left(t), in)
      def success00(u: U): Eval[R] = onSuccess0(Right(u))
      def success01(u: U, in: ParseInput): Eval[R] = onSuccess(Right(u), in)

      Eval.defer(p.apply(success0, failure0, success1, failure0, input))
    }
  }

  def concat[U](q: Parser[U]): Parser[T ~ U] = new Parser[T ~ U] {
    def apply[R](onSuccess0: T ~ U => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (T ~ U, ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] = {
      def success0(t: T): Eval[R] =
        Eval.defer(q.apply(success00(t), onFailure0, success11(t), onFailure, input))
      def success00(t: T)(u: U): Eval[R] = onSuccess0((t, u))
      def success1(t: T, in: ParseInput): Eval[R] =
        Eval.defer(q.apply(success10(t, in), onFailure, success11(t), onFailure, in))
      def success10(t: T, in: ParseInput)(u: U): Eval[R] = onSuccess((t, u), in)
      def success11(t: T)(u: U, in: ParseInput): Eval[R] = onSuccess((t, u), in)

      Eval.defer(p.apply(success0, onFailure0, success1, onFailure, input))
    }
  }

  def repeat: Parser[List[T]] = new Parser[List[T]] {
    def apply[R](onSuccess0: List[T] => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (List[T], ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] = {
      def success0(t: T): Eval[R] = onSuccess0(List.empty)
      def success1(t: T, in: ParseInput): Eval[R] =
        Eval.defer(apply(success10(t, in), onFailure0, success11(t), onFailure, in))
      def success10(t: T, in: ParseInput)(ts: List[T]): Eval[R] =
        Eval.defer(onSuccess(t :: ts, in))
      def success11(t: T)(ts: List[T], in: ParseInput): Eval[R] =
        Eval.defer(onSuccess(t :: ts, in))
      def failure(err: ParseError): Eval[R] = onSuccess0(List.empty)

      Eval.defer(p.apply(success0, failure, success1, failure, input))
    }
  }

  def optional: Parser[Option[T]] = new Parser[Option[T]] {
    def apply[R](onSuccess0: Option[T] => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (Option[T], ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] = {
      def success0(t: T): Eval[R] = onSuccess0(Some(t))
      def success(t: T, in: ParseInput): Eval[R] = onSuccess(Some(t), in)
      def failure(err: ParseError): Eval[R] = onSuccess0(None)

      Eval.defer(p.apply(success0, failure, success, failure, input))
    }
  }

  def ~[U](q: Parser[U])(implicit TU: Concat[T, U]): Parser[TU.Out] =
    p.concat(q).map { case (t, u) => TU.concat(t, u) }

  def *(implicit T: Repeat[T]): Parser[T.Out] =
    p.repeat.map(T.repeat(_))

  def ?(implicit T: Optional[T]): Parser[T.Out] =
    p.optional.map(T.optional(_))

  def `-` : Parser[Unit] = p.map(_ => ())

  def map[U](f: T => U): Parser[U] = new Parser[U] {
    def apply[R](onSuccess0: U => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (U, ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] = {
      def success0(t: T): Eval[R] = Eval.defer(onSuccess0(f(t)))
      def success1(t: T, in: ParseInput): Eval[R] = Eval.defer(onSuccess(f(t), in))

      Eval.defer(p.apply(success0, onFailure0, success1, onFailure, input))
    }
  }

  def flatMap[U](f: T => Parser[U]): Parser[U] = new Parser[U] {
    def apply[R](onSuccess0: U => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (U, ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] = {
      def success0(t: T): Eval[R] =
        Eval.later(f(t)).flatMap(_.apply(onSuccess0, onFailure0, onSuccess, onFailure, input))
      def success1(t: T, in: ParseInput): Eval[R] =
        Eval.later(f(t)).flatMap(_.apply(onSuccess(_, in), onFailure, onSuccess, onFailure, in))

      Eval.defer(p.apply(success0, onFailure0, success1, onFailure, input))
    }
  }

  def name(s: String): Parser[T] = new Parser[T] {
    def apply[R](onSuccess0: T => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (T, ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] = {
      def failure0(err: ParseError): Eval[R] = onFailure0(Unexpected(Set(s), err.position))
      def failure(err: ParseError): Eval[R] = onFailure(Unexpected(Set(s), err.position))

      Eval.defer(p.apply(onSuccess0, failure0, onSuccess, failure, input))
    }
  }
}

object Parser {
  def apply[T](f: ParseInput => Result[T]): Parser[T] = new Parser[T] {
    def apply[R](onSuccess0: T => Eval[R],
                 onFailure0: ParseError => Eval[R],
                 onSuccess: (T, ParseInput) => Eval[R],
                 onFailure: ParseError => Eval[R],
                 input: ParseInput): Eval[R] =
      f(input) match {
        case Success(t, in) =>
          if (input.position === in.position) onSuccess0(t) else onSuccess(t, in)
        case Failure(err) =>
          if (input.position === err.position) onFailure0(err) else onFailure(err)
      }
  }

  def satisfy(p: Char => Boolean): Parser[Char] = Parser { input =>
    if (input.view.headOption.exists(p)) Success(input.view.head, input.advance)
    else Failure(Unexpected(Set.empty, input.position))
  }

  def char(c: Char): Parser[Char] = Parser { input =>
    if (input.view.headOption.exists(_ == c)) Success(input.view.head, input.advance)
    else Failure(Unexpected(Set(s"'$c'"), input.position))
  }

  def string(s: String): Parser[String] = Parser { input =>
    if (input.view.take(s.length).mkString == s) Success(s, input.advance(s.length))
    else Failure(Unexpected(Set(s"${'"'}${s}${'"'}"), input.position))
  }

  val eof: Parser[Unit] = Parser { input =>
    if (input.view.isEmpty) Success((), input)
    else Failure(Unexpected(Set("<eof>"), input.position))
  }
}
