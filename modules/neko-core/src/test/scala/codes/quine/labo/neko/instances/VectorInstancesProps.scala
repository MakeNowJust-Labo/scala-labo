package codes.quine.labo
package neko
package instances

import scalaprops._
import data._, props._

object VectorInstancesProps extends Scalaprops {
  implicit val stringGenInstance: Gen[String] = Gen.asciiString
  implicit def idGenInstance[A](implicit ga: Gen[A]): Gen[Id[A]] = Gen[A].map(Id(_))

  val laws = Properties.list(
    OrdProps[Vector[Int]].all,
    HashProps[Vector[Int]].all,
    MonoidProps[Vector[Int]].all,
    MonadProps[Vector].all[Int, Int, Int],
    CoflatMapProps[Vector].all[Int, Int, Int],
    AlternativeProps[Vector].all[Int, Int, Int],
    TraverseProps[Vector].all[Id, Id, Int, Int, Int, String]
  )
}
