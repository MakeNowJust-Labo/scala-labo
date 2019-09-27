package codes.quine.labo
package reify

import neko._, data._

sealed trait TermF[A]
final case class AndF[A](l: A, r: A) extends TermF[A]
final case class OrF[A](l: A, r: A) extends TermF[A]
final case class ImplyF[A](l: A, r: A) extends TermF[A]
final case class NotF[A](n: A) extends TermF[A]
final case class VarF[A](name: String) extends TermF[A]

object TermF {
  implicit val termFTraverseInstance: Traverse[TermF] = new Traverse[TermF] {
    def foldLeft[A, B](fa: TermF[A], b: B)(f: (B, A) => B): B = fa match {
      case AndF(l, r)   => f(f(b, l), r)
      case OrF(l, r)    => f(f(b, l), r)
      case ImplyF(l, r) => f(f(b, l), r)
      case NotF(n)      => f(b, n)
      case VarF(_)      => b
    }
    def foldRight[A, B](fa: TermF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case AndF(l, r)   => Eval.defer(f(l, Eval.defer(f(r, lb))))
      case OrF(l, r)    => Eval.defer(f(l, Eval.defer(f(r, lb))))
      case ImplyF(l, r) => Eval.defer(f(l, Eval.defer(f(r, lb))))
      case NotF(n)      => Eval.defer(f(n, lb))
      case VarF(_)      => lb
    }

    def traverse[G[_]: Applicative, A, B](fa: TermF[A])(f: A => G[B]): G[TermF[B]] = fa match {
      case AndF(l, r)   => Applicative[G].map2(f(l), f(r))(AndF(_, _))
      case OrF(l, r)    => Applicative[G].map2(f(l), f(r))(OrF(_, _))
      case ImplyF(l, r) => Applicative[G].map2(f(l), f(r))(ImplyF(_, _))
      case NotF(n)      => Applicative[G].map(f(n))(NotF(_))
      case VarF(name)   => Applicative[G].pure(VarF(name))
    }
  }
}

final case class Term(phi: TermF[Term]) { p =>
  def &&(q: Term): Term = Term(AndF(p, q))
  def ||(q: Term): Term = Term(OrF(p, q))
  def unary_~(): Term = Term(NotF(p))
  def ->(q: Term): Term = Term(ImplyF(p, q))
  def <->(q: Term): Term = (p -> q) && (q -> p)
}

object Term {
  implicit val termMuRefInstance: MuRef.Aux[Term, TermF] = new MuRef[Term] {
    type DeRef[A] = TermF[A]

    def mapDeRef[F[_]: Applicative, R](a: Term)(f: MuRefFunction[F, TermF, R]): F[TermF[R]] =
      Traverse[TermF].traverse(a.phi)(a => f(a))
  }

  def Var(name: String): Term = Term(VarF(name))
}
