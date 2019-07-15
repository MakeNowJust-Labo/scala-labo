package codes.quine.labo.nyan.props

import scala.collection.mutable

final class NyanLaw private (val ord: Int, val fullName: String, val simpleName: String) {
  override def hashCode: Int = ord
  override def toString: String = simpleName
}

object NyanLaw {
  private[this] val set: mutable.Set[NyanLaw] = mutable.Set.empty

  private[this] def law(fullName: String, simpleName: String = ""): NyanLaw = {
    set.synchronized {
      val ord = set.size
      val l = new NyanLaw(ord, fullName, if (simpleName.isEmpty) fullName else simpleName)
      set += l
      l
    }
  }

  private[this] def law0(clazz: NyanLaw, simpleName: String): NyanLaw =
    law(s"${clazz.simpleName} $simpleName", simpleName)

  val functor: NyanLaw = law("functor")
  val functorIdentity: NyanLaw = law0(functor, "identity")
  val functorComposition: NyanLaw = law0(functor, "composition")

  val applicative: NyanLaw = law("applicative")
  val applicativeIdentity: NyanLaw = law0(applicative, "identity")
  val applicativeHomomorphism: NyanLaw = law0(applicative, "homomorphism")
  val applicativeInterchange: NyanLaw = law0(applicative, "interchange")
  val applicativeComposition: NyanLaw = law0(applicative, "composition")
  val applicativeMap: NyanLaw = law0(applicative, "map")

  val monad: NyanLaw = law("monad")
  val monadLeftIdentity: NyanLaw = law0(monad, "left identity")
  val monadRightIdentity: NyanLaw = law0(monad, "right identity")
  val monadAssociativity: NyanLaw = law0(monad, "associativity")
  val monadTailRecMStackSafety: NyanLaw = law0(monad, "tailRecM stack safety")

  val monoidK: NyanLaw = law("monoidK")
  val monoidKLeftIdentity: NyanLaw = law0(monoidK, "left identity")
  val monoidKRightIdentity: NyanLaw = law0(monoidK, "right identity")
  val monoidKAssociativity: NyanLaw = law0(monoidK, "associativity")

  val alternative: NyanLaw = law("alternative")
  val alternativeRightAbsorption: NyanLaw = law0(alternative, "right absorption")
  val alternativeLeftDistributivity: NyanLaw = law0(alternative, "left distributivity")
  val alternativeRightDistributivity: NyanLaw = law0(alternative, "right distributivity")
}