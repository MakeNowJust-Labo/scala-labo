package codes.quine.labo
package reify

case class Unique(id: Int) {
  def next: Unique = Unique(id + 1)
}

object Unique {
  def zero: Unique = Unique(0)
}
