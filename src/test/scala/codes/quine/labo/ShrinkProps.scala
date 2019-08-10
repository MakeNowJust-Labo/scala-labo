package codes.quine.labo

import scalaprops._
import fun._

object ShrinkProps extends Scalaprops {
  def prop[A](implicit s: Shrink[A]): A => Boolean =
    (a: A) => !s(a).contains(a)

  val boolean = Property.forAll(prop[Boolean])
  val int = Property.forAll(prop[Int])
  val listInt = Property.forAll(prop[List[Int]])
  val listUnit = Property.forAll(prop[List[Unit]])
  val listBoolean = Property.forAll(prop[List[Boolean]])
}