package codes.quine.labo.nyan
package instances

trait BooleanInstances {
  implicit val BooleanEqInstances: Eq[Boolean] = Eq.default
}
