package codes.quine.labo
package fa

import scalaprops._
import TestTWDFA._

object TWDFAProps extends Scalaprops {
  val convert = Property.forAll((xs: List[Σ]) => M.run(xs) == D.run(xs))
}
