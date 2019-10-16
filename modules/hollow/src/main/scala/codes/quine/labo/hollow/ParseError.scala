package codes.quine.labo
package hollow

import neko.data.Ordering

import ParseError._

sealed trait ParseError { self =>
  def message: String
  def position: Position
  def fullMessage: String = s"${position.filename}:${position.line}:${position.column}: $message"

  def merge(that: ParseError): ParseError = self.position.tryCmp(that.position) match {
    case None              => self // TODO: undefined behavior
    case Some(Ordering.LT) => self
    case Some(Ordering.GT) => that
    case Some(Ordering.EQ) =>
      (self, that) match {
        case (s: Fail, _)                           => s
        case (_, t: Fail)                           => t
        case (Unexpected(u1, p), Unexpected(u2, _)) => Unexpected(u1 | u2, p)
      }
  }
}

object ParseError {
  final case class Fail(message: String, position: Position) extends ParseError
  final case class Unexpected(unexpected: Set[String], position: Position) extends ParseError {
    def message: String = unexpected.toList.sorted.mkString("unexpected: ", ", ", "")
  }

  def unapply(err: ParseError): Option[(String, Position)] = Some((err.message, err.position))
}
