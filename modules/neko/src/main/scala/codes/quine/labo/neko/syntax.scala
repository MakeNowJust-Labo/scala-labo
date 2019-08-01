package codes.quine.labo
package neko

trait AllSyntax
    extends Alternative.ToAlternativeOps
    with Applicative.ToApplicativeOps
    with Arrow.ToArrowOps
    with Category.ToCategoryOps
    with Comonad.ToComonadOps
    with Contravariant.ToContravariantOps
    with Defer.ToDeferOps
    with Eq.ToEqOps
    with Functor.ToFunctorOps
    with Hash.ToHashOps
    with Monad.ToMonadOps
    with Monoid.ToMonoidOps
    with MonoidK.ToMonoidKOps
    with Ord.ToOrdOps
    with PartialOrd.ToPartialOrdOps
    with Semigroup.ToSemigroupOps
    with SemigroupK.ToSemigroupKOps

object syntax extends AllSyntax
