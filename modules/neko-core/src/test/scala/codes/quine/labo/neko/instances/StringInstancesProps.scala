package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object StringInstancesProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString

  val laws = Properties.list(
    OrdProps[String].all,
    HashProps[String].all,
    MonoidProps[String].all
  )
}
