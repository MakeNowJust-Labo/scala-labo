package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object IntInstancesProps extends Scalaprops {
  val laws = Properties.list(
    OrdProps[Int].all,
    HashProps[Int].all
  )

  val `laws (sum)` = {
    implicit val intMonoidInstance: Monoid[Int] = int.sumMonoidInstance
    Properties.list(
      MonoidProps[Int].all
    )
  }

  val `laws (product)` = {
    implicit val intMonoidInstance: Monoid[Int] = int.productMonoidInstance
    Properties.list(
      MonoidProps[Int].all
    )
  }
}
