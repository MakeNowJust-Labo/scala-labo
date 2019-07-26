package codes.quine.labo.neko
package instances

trait BooleanInstances {
  implicit val BooleanEqInstances: Eq[Boolean] = Eq.default
}
