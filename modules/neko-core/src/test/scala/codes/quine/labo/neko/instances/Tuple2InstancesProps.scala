package codes.quine.labo
package neko
package instances

import scalaprops._
import props._

object Tuple2InstancesProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString

  val laws = Properties.list(
    OrdProps[(Int, String)].all,
    HashProps[(Int, String)].all,
    MonoidProps[(String, List[Int])].all
  )
}
