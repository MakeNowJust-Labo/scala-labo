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

  val fold: NekoLaw = law("fold")
  val foldAll: NekoLaw = all(fold)
  val foldFoldRightLazy: NekoLaw = law0(fold, "foldRight lazy")
  val foldLeftConsistencyWithFoldMap: NekoLaw = law0(fold, "left consistency with foldMap")
  val foldRightConsistencyWithFoldMap: NekoLaw = law0(fold, "right consistency with foldMap")

  val traverse: NekoLaw = law("traverse")
  val traverseAll: NekoLaw = all(traverse)
  val traverseIdentity: NekoLaw = law0(traverse, "identity")
  val traverseFoldMap: NekoLaw = law0(traverse, "foldMap")
  val traverseFoldLeft: NekoLaw = law0(traverse, "foldLeft")
  val traverseFoldRight: NekoLaw = law0(traverse, "foldRight")
  val traverseSequentialComposition: NekoLaw = law0(traverse, "sequential composition")
  val traverseParallelComposition: NekoLaw = law0(traverse, "parallel composition")

  val functor: NekoLaw = law("functor")
  val functorAll: NekoLaw = all(functor)
  val functorIdentity: NekoLaw = law0(functor, "identity")
  val functorComposition: NekoLaw = law0(functor, "composition")

  val apply: NekoLaw = law("apply")
  val applyAll: NekoLaw = all(apply)
  val applyComposition: NekoLaw = law0(apply, "composition")

  val applicative: NekoLaw = law("applicative")
  val applicativeAll: NekoLaw = all(applicative)
  val applicativeIdentity: NekoLaw = law0(applicative, "identity")
  val applicativeHomomorphism: NekoLaw = law0(applicative, "homomorphism")
  val applicativeInterchange: NekoLaw = law0(applicative, "interchange")
  val applicativeMap: NekoLaw = law0(applicative, "map")

  val flatMap: NekoLaw = law("flatMap")
  val flatMapAll: NekoLaw = all(flatMap)
  val flatMapAssociativity: NekoLaw = law0(flatMap, "associativity")
  val flatMapTailRecMConsistency: NekoLaw = law0(flatMap, "tailRecM consistency")

  val monad: NekoLaw = law("monad")
  val monadAll: NekoLaw = all(monad)
  val monadLeftIdentity: NekoLaw = law0(monad, "left identity")
  val monadRightIdentity: NekoLaw = law0(monad, "right identity")
  val monadAssociativity: NekoLaw = law0(monad, "associativity")
  val monadTailRecMStackSafety: NekoLaw = law0(monad, "tailRecM stack safety")

  val coflatMap: NekoLaw = law("coflatMap")
  val coflatMapAll: NekoLaw = all(coflatMap)
  val coflatMapAssociativity: NekoLaw = law0(coflatMap, "associativity")
  val coflatMapCoflattenThroughMap: NekoLaw = law0(coflatMap, "coflatten through map")
  val coflatMapCoherence: NekoLaw = law0(coflatMap, "coherence")
  val coflatMapIdentity: NekoLaw = law0(coflatMap, "identity")

  val comonad: NekoLaw = law("comonad")
  val comonadAll: NekoLaw = all(comonad)
  val comonadExtractCoflattenIdentity: NekoLaw = law0(comonad, "extract coflatten identity")
  val comonadMapCoflattenIdentity: NekoLaw = law0(comonad, "map coflatten identity")
  val comonadMapCoflatMapCoherence: NekoLaw = law0(comonad, "map coflatMap coherence")
  val comonadLeftIdentity: NekoLaw = law0(comonad, "left identity")
  val comonadRightIdnetity: NekoLaw = law0(comonad, "right identity")

  val bimonad: NekoLaw = law("bimonad")
  val bimonadAll: NekoLaw = all(bimonad)
  val bimonadExtractPureIdentity: NekoLaw = law0(bimonad, "extract pure identity")
  val bimonadExtractFlatMapEntwining: NekoLaw = law0(bimonad, "extract flatMap entwining")
  val bimonadPureCoflatMapEntwining: NekoLaw = law0(bimonad, "pure coflatMap entwining")

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

  val compose: NekoLaw = law("compose")
  val composeAll: NekoLaw = all(compose)
  val composeAssociativity: NekoLaw = law0(compose, "associativity")

  val category: NekoLaw = law("category")
  val categoryAll: NekoLaw = all(category)
  val categoryLeftIdentity: NekoLaw = law0(category, "left identity")
  val categoryRightIdentity: NekoLaw = law0(category, "right identity")

  val profunctor: NekoLaw = law("profunctor")
  val profunctorAll: NekoLaw = all(profunctor)
  val profunctorDimapIdentity: NekoLaw = law0(profunctor, "dimap identity")
  val profunctorDimapComposition: NekoLaw = law0(profunctor, "dimap composition")

  val strong: NekoLaw = law("strong")
  val strongAll: NekoLaw = all(strong)
  val strongFirstSecondDimap: NekoLaw = law0(strong, "first second dimap")
  val strongSecondFirstDimap: NekoLaw = law0(strong, "second first dimap")
  val strongLmapFirstRmap: NekoLaw = law0(strong, "lmap first rmap")
  val strongLmapSecondRmap: NekoLaw = law0(strong, "lmap second rmap")
  val strongFirstDinaturality: NekoLaw = law0(strong, "first dinaturality")
  val strongSecondDinaturality: NekoLaw = law0(strong, "second dinaturality")
  val strongFirstFirstDimap: NekoLaw = law0(strong, "first first dimap")
  val strongSecondSecondDimap: NekoLaw = law0(strong, "second second dimap")

  val arrow: NekoLaw = law("arrow")
  val arrowAll: NekoLaw = all(arrow)
  val arrowIdentity: NekoLaw = law0(arrow, "identity")
  val arrowComposition: NekoLaw = law0(arrow, "composition")
  val arrowExtension: NekoLaw = law0(arrow, "extension")
  val arrowFunctor: NekoLaw = law0(arrow, "functor")
  val arrowExchange: NekoLaw = law0(arrow, "exchange")
  val arrowUnit: NekoLaw = law0(arrow, "unit")
  val arrowAssociativity: NekoLaw = law0(arrow, "associativity")
  val arrowSplitConsistency: NekoLaw = law0(arrow, "split consistency")
  val arrowMergeConsistency: NekoLaw = law0(arrow, "merge consistency")
}
