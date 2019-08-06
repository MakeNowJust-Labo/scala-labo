package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object BooleanInstancesProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Boolean].all,
    HashProps[Boolean].all
  )

  val `laws (all)` = {
    implicit val booleanMonoidInstance: Monoid[Boolean] = boolean.allMonoidInstance
    Properties.list(
      MonoidProps[Boolean].all
    )
  }

  val `laws (any)` = {
    implicit val booleanMonoidInstance: Monoid[Boolean] = boolean.anyMonoidInstance
    Properties.list(
      MonoidProps[Boolean].all
    )
  }
}
