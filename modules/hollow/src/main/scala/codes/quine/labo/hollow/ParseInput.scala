package codes.quine.labo
package hollow

import scala.collection.View
import scala.annotation.tailrec

final case class ParseInput(source: String, view: View[Char], position: Position) {
  def advance: ParseInput = ParseInput(source, view.tail, position.advance(view.headOption))

  @tailrec
  def advance(n: Int): ParseInput =
    if (n <= 0) this else advance.advance(n - 1)
}

object ParseInput {
  def apply(source: String, filename: String = "[source]"): ParseInput =
    ParseInput(source, source.view, Position(filename, 1, 1))
}
