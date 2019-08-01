package codes.quine.labo
package neko
package props

import scalaprops._

import laws._, instances._

trait HashProps[A] extends EqProps[A] {
  val laws: HashLaws[A]

  def hashCompatibility(implicit ga: Gen[A]): Property =
    Property.forAll(laws.hashCompatibility(_: A, _: A))

  def hash(implicit ga: Gen[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.hash)(
      NekoLaw.hashCompatibility -> hashCompatibility
    )
}

object HashProps {
  def apply[A: Hash]: HashProps[A] = new HashProps[A] {
    val laws: HashLaws[A] = HashLaws[A]
  }
}
