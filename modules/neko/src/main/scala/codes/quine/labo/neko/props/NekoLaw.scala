package codes.quine.labo
package neko
package props

import scala.collection.mutable

final class NekoLaw private (val ord: Int, val fullName: String, val simpleName: String) {
  override def hashCode: Int = ord
  override def toString: String = simpleName
}

object NekoLaw {
  private[this] val set: mutable.Set[NekoLaw] = mutable.Set.empty

  private[this] def law(fullName: String, simpleName: String = ""): NekoLaw =
    set.synchronized {
      val ord = set.size
      val l = new NekoLaw(ord, fullName, if (simpleName.isEmpty) fullName else simpleName)
      set += l
      l
    }

  private[this] def all(clazz: NekoLaw): NekoLaw =
    law(s"${clazz.simpleName} all", clazz.simpleName)

  private[this] def law0(clazz: NekoLaw, simpleName: String): NekoLaw =
    law(s"${clazz.simpleName} $simpleName", simpleName)

  val eq: NekoLaw = law("eq")
  val eqAll: NekoLaw = all(eq)
  val eqReflexivity: NekoLaw = law0(eq, "reflexivity")
  val eqSymmetry: NekoLaw = law0(eq, "symmetry")
  val eqAntiSymmetry: NekoLaw = law0(eq, "anti symmetry")
  val eqTransivity: NekoLaw = law0(eq, "transivity")

  val partialOrd: NekoLaw = law("partialOrd")
  val partialOrdAll: NekoLaw = all(partialOrd)
  val partialOrdReflexivity: NekoLaw = law0(partialOrd, "reflexivity")
  val partialOrdAntisymmetry: NekoLaw = law0(partialOrd, "anti symmetry")
  val partialOrdTransivity: NekoLaw = law0(partialOrd, "transivity")

  val ord: NekoLaw = law("ord")
  val ordAll: NekoLaw = all(ord)
  val ordTotality: NekoLaw = law0(ord, "totality")

  val hash: NekoLaw = law("hash")
  val hashAll: NekoLaw = all(hash)
  val hashCompatibility: NekoLaw = law0(hash, "compatibility")

  val semigroup: NekoLaw = law("semigroup")
  val semigroupAll: NekoLaw = all(semigroup)
  val semigroupAssociativity: NekoLaw = law0(semigroup, "associativity")

  val monoid: NekoLaw = law("monoid")
  val monoidAll: NekoLaw = all(monoid)
  val monoidLeftIdentity: NekoLaw = law0(monoid, "left identity")
  val monoidRightIdentity: NekoLaw = law0(monoid, "right identity")

  val defer: NekoLaw = law("defer")
  val deferAll: NekoLaw = all(defer)
  val deferIdentity: NekoLaw = law0(defer, "identity")
  val deferDoesNotEvaluate: NekoLaw = law0(defer, "does not evaluate")
  val deferStackSafety: NekoLaw = law0(defer, "stack safety")

  val functor: NekoLaw = law("functor")
  val functorAll: NekoLaw = all(functor)
  val functorIdentity: NekoLaw = law0(functor, "identity")
  val functorComposition: NekoLaw = law0(functor, "composition")

  val applicative: NekoLaw = law("applicative")
  val applicativeAll: NekoLaw = all(applicative)
  val applicativeIdentity: NekoLaw = law0(applicative, "identity")
  val applicativeHomomorphism: NekoLaw = law0(applicative, "homomorphism")
  val applicativeInterchange: NekoLaw = law0(applicative, "interchange")
  val applicativeComposition: NekoLaw = law0(applicative, "composition")
  val applicativeMap: NekoLaw = law0(applicative, "map")

  val monad: NekoLaw = law("monad")
  val monadAll: NekoLaw = all(monad)
  val monadLeftIdentity: NekoLaw = law0(monad, "left identity")
  val monadRightIdentity: NekoLaw = law0(monad, "right identity")
  val monadAssociativity: NekoLaw = law0(monad, "associativity")
  val monadTailRecMStackSafety: NekoLaw = law0(monad, "tailRecM stack safety")

  val contravariant: NekoLaw = law("contravariant")
  val contravariantAll: NekoLaw = all(contravariant)
  val contravariantIdentity: NekoLaw = law0(contravariant, "identity")
  val contravariantComposition: NekoLaw = law0(contravariant, "composition")

  val semigroupK: NekoLaw = law("semigroupK")
  val semigroupKAll: NekoLaw = all(semigroupK)
  val semigroupKAssociativity: NekoLaw = law0(semigroupK, "associativity")

  val monoidK: NekoLaw = law("monoidK")
  val monoidKAll: NekoLaw = all(monoidK)
  val monoidKLeftIdentity: NekoLaw = law0(monoidK, "left identity")
  val monoidKRightIdentity: NekoLaw = law0(monoidK, "right identity")

  val alternative: NekoLaw = law("alternative")
  val alternativeAll: NekoLaw = all(alternative)
  val alternativeRightAbsorption: NekoLaw = law0(alternative, "right absorption")
  val alternativeLeftDistributivity: NekoLaw = law0(alternative, "left distributivity")
  val alternativeRightDistributivity: NekoLaw = law0(alternative, "right distributivity")
}
