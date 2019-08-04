package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object StringInstancesProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString

  val laws = Properties.list(
    EqProps[String].eq,
    PartialOrdProps[String].partialOrd,
    OrdProps[String].ord,
    HashProps[String].hash,
    SemigroupProps[String].semigroup,
    MonoidProps[String].monoid
  )
}
