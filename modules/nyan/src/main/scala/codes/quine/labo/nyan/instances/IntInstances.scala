package codes.quine.labo.nyan
package instances

trait IntInstances {
  implicit val IntEqInstances: Eq[Int] = Eq.default
}
