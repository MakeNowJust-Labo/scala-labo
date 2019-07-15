package codes.quine.labo.nyan
package instances

trait IntInstances {
  implicit object IntInstances extends Eq[Int] {
    def eqv(x: Int, y: Int): Boolean = x == y

    override def neqv(x: Int, y: Int): Boolean = x != y
  }
}
