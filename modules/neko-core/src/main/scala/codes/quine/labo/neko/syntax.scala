package codes.quine.labo
package neko

trait AllSyntax
    extends Alternative.ToAlternativeOps
    with Applicative.ToApplicativeOps
    with Apply.ToApplyOps
    with Arrow.ToArrowOps
    with Bimonad.ToBimonadOps
    with Category.ToCategoryOps
    with CoflatMap.ToCoflatMapOps
    with Comonad.ToComonadOps
    with Compose.ToComposeOps
    with Contravariant.ToContravariantOps
    with Defer.ToDeferOps
    with Eq.ToEqOps
    with FlatMap.ToFlatMapOps
    with Functor.ToFunctorOps
    with Hash.ToHashOps
    with Monad.ToMonadOps
    with Monoid.ToMonoidOps
    with MonoidK.ToMonoidKOps
    with Ord.ToOrdOps
    with PartialOrd.ToPartialOrdOps
    with Profunctor.ToProfunctorOps
    with Semigroup.ToSemigroupOps
    with SemigroupK.ToSemigroupKOps
    with Strong.ToStrongOps

object syntax extends AllSyntax