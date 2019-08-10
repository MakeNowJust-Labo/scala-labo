package codes.quine.labo

import scalaprops._

object BoolProps extends Scalaprops {
  def prop = Property.forAllS[Boolean](_ == true)
}