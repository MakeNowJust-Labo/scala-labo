package codes.quine.labo.neko
package instances

trait IntInstances {
  implicit val IntEqInstances: Eq[Int] = Eq.default
}
