package codes.quine.labo
package neko
package props

import scalaprops._
import instances._, laws._

trait HashProps[A] {
  val laws: HashLaws[A]
  import laws._

  def hashCompatibility(implicit ga: Gen[A]): Property =
    Property.forAll(laws.hashCompatibility(_: A, _: A))

  def props(implicit ga: Gen[A]): Properties[NekoLaw] =
    Properties.properties(NekoLaw.hash)(
      NekoLaw.hashCompatibility -> hashCompatibility
    )

  def all(implicit ga: Gen[A], gf: Gen[A => A]): Properties[NekoLaw] =
    Properties.fromProps(NekoLaw.hashAll, EqProps[A].all, props)
}

object HashProps {
  def apply[A: Hash]: HashProps[A] = new HashProps[A] {
    val laws: HashLaws[A] = HashLaws[A]
  }
}
