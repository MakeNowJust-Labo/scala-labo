package codes.quine.labo
package hollow

import neko.PartialOrd
import neko.syntax._
import neko.instances.int._
import neko.data.Ordering

final case class Position(filename: String, line: Int, column: Int) { self =>
  def advance(c: Option[Char]): Position = c match {
    case Some('\n') => Position(filename, line + 1, 1)
    case _          => Position(filename, line, column + 1)
  }

  def tryCmp(that: Position): Option[Ordering] = (self, that) match {
    case (s, t) if s.filename != t.filename => None
    case (s, t) =>
      Some((self.line <=> that.line) |+| (self.column <=> that.column))
  }
}

object Position extends PositionInstances0

private[hollow] trait PositionInstances0 {
  implicit val positionPartialOrdInstance: PartialOrd[Position] = new PartialOrd[Position] {
    def tryCmp(x: Position, y: Position): Option[Ordering] = x.tryCmp(y)
  }
}
