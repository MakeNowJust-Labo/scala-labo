package codes.quine.labo

import nyan._, category._, data._, free._, syntax._

sealed trait CharIO[A]

case object CharRead extends CharIO[Char]
case class CharWrite(c: Char) extends CharIO[Unit]

object CharIO {
  type Program[A] = Operational[CharIO, A]
  val Program = Monad[Program]

  def read: Program[Char] = Operational.lift(CharRead)

  def write(c: Char): Program[Unit] = Operational.lift(CharWrite(c))

  def readString: Program[String] =
    Program.tailRecM("") { s =>
      read.map { c =>
        if (c == '\u0000' || c == '\n') Right(s) else Left(s + c)
      }
    }

  def writeString(s: String): Program[Unit] =
    Program.tailRecM(s) { s =>
      if (s.isEmpty) Program.pure(Right(())) else write(s.head).map(_ => Left(s.tail))
    }

  def run[A](p: Program[A])(input: String): ((String, String), A) =
    p.interpret(new (CharIO ~> State[(String, String), ?]) {
        def apply[B](fb: CharIO[B]): State[(String, String), B] =
          fb match {
            case CharRead =>
              for {
                (i, o) <- State.read[(String, String)]
                _ <- State.write((i.tail, o))
              } yield i.headOption.getOrElse('\u0000')
            case CharWrite(c) =>
              for {
                (i, o) <- State.read[(String, String)]
                _ <- State.write((i, o + c))
              } yield ()
          }
      })
      .run((input, ""))
      .run
}
