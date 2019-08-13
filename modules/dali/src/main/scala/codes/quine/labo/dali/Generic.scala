package codes.quine.labo
package dali

import scala.language.experimental.macros
import scala.reflect.macros.{blackbox, whitebox}

trait Generic[A] {
  type Repr
  def embed(a: A): Repr
  def eject(r: Repr): A
}

object Generic {
  type Aux[A, R] = Generic[A] { type Repr = R }

  def apply[A](implicit g: Generic[A]): Aux[A, g.Repr] = g

  implicit def materialize[A, R]: Aux[A, R] = macro GenericMacros.materialize[A, R]
}

private[dali] trait ReprTypes {
  val c: blackbox.Context
  import c.universe._

  val hlistTpe: Type = typeOf[HList]
  val hnilTpe: Type = typeOf[HNil]
  val hconsTpe: Type = typeOf[:*:[_, _]].typeConstructor
  val coproductTpe: Type = typeOf[Coproduct]
  val cnilTpe: Type = typeOf[CNil]
  val cconsTpe: Type = typeOf[:+:[_, _]].typeConstructor
}

class GenericMacros(val c: whitebox.Context) extends ReprTypes {
  import c.universe._

  def materialize[A: WeakTypeTag, R: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    if (isReprType(tpe)) {
      abort("no Generic instance available for HList or Coproduct")
    }

    if (isProduct(tpe)) mkProductGeneric(tpe)
    else if (isCoproduct(tpe)) mkCoproductGeneric(tpe)
    else abort(s"no Generic instance available for ${tpe.typeSymbol}")
  }

  def abort(msg: String): Nothing =
    c.abort(c.enclosingPosition, msg)

  def classSym(tpe: Type): ClassSymbol = {
    val sym = tpe.typeSymbol
    if (!sym.isClass) {
      abort(s"$sym is not a class or trait")
    }

    val classSym = sym.asClass
    classSym.typeSignature

    classSym
  }

  def isReprType(tpe: Type): Boolean =
    tpe <:< hlistTpe || tpe <:< coproductTpe

  def isProduct(tpe: Type): Boolean =
    tpe.typeSymbol.isClass && classSym(tpe).isCaseClass

  def isCoproduct(tpe: Type): Boolean = {
    if (!tpe.typeSymbol.isClass) {
      return false
    }

    val sym = classSym(tpe)
    if (!sym.isSealed) {
      return false
    }

    val subclasses = sym.knownDirectSubclasses.toList.map { child =>
      child.asClass.typeSignature
    }
    subclasses.nonEmpty && subclasses.forall { child =>
      isProduct(child) || isCoproduct(child)
    }
  }

  def fieldsOf(tpe: Type): List[(TermName, Type)] =
    tpe.decls.sorted.collect {
      case sym: TermSymbol if sym.isCaseAccessor && sym.isGetter =>
        (sym.name.toTermName, sym.typeSignatureIn(tpe).finalResultType)
    }

  def mkProductGeneric(tpe: Type): Tree = {
    if (classSym(tpe).isModuleClass) {
      return mkSingletonGeneric(tpe)
    }

    val fields = fieldsOf(tpe)
    val elems = fields.map { case (name, tpe) => (TermName(c.freshName("pat")), tpe) }
    val companion = tpe.typeSymbol.companion
    val construct = q"$companion(..${elems.map(_._1)})"
    val pattern = pq"$companion(..${elems.map { case (binder, tpe) => pq"$binder" }})"
    val reprConstruct = elems.foldRight(q"_root_.codes.quine.labo.dali.HNil": Tree) {
      case ((bound, _), acc) => q"_root_.codes.quine.labo.dali.:*:($bound, $acc)"
    }
    val reprPattern =
      elems.foldRight(pq"_root_.codes.quine.labo.dali.HNil": Tree) {
        case ((bound, _), acc) => pq"_root_.codes.quine.labo.dali.:*:($bound, $acc)"
      }
    val reprTpe =
      elems.foldRight(tq"_root_.codes.quine.labo.dali.HNil": Tree) {
        case ((_, tpe), acc) => tq"_root_.codes.quine.labo.dali.:*:[$tpe, $acc]"
      }

    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.codes.quine.labo.dali.Generic[$tpe] {
        type Repr = $reprTpe
        def embed(a: $tpe): Repr = (a match { case $pattern => $reprConstruct })
        def eject(r: Repr): $tpe = (r match { case $reprPattern => $construct })
      }
      new $clsName: _root_.codes.quine.labo.dali.Generic.Aux[$tpe, $reprTpe]
    """
  }

  def mkSingletonGeneric(tpe: Type): Tree = {
    val singleton = tpe match {
      case SingleType(tpe, sym) => sym
      case _                    => abort(s"BUG: $tpe is not singleton")
    }

    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.codes.quine.labo.dali.Generic[$tpe] {
        type Repr = _root_.codes.quine.labo.dali.HNil
        def embed(a: $tpe): Repr = _root_.codes.quine.labo.dali.HNil
        def eject(r: Repr): $tpe = $singleton
      }
      new $clsName: _root_.codes.quine.labo.dali.Generic.Aux[$tpe, _root_.codes.quine.labo.dali.HNil]
    """
  }

  def mkCoproductGeneric(tpe: Type): Tree = {
    val sym = classSym(tpe)
    val subclasses = sym.knownDirectSubclasses.toList.map { child =>
      child.asClass.typeSignature.typeSymbol
    }

    val reprTpe = subclasses.foldRight(tq"_root_.codes.quine.labo.dali.CNil": Tree) {
      case (tpe, acc) => tq"_root_.codes.quine.labo.dali.:+:[$tpe, $acc]"
    }
    val cases = subclasses.map { tpe =>
      cq"c: $tpe => _root_.codes.quine.labo.dali.Coproduct[$reprTpe](c)"
    }

    val clsName = TypeName(c.freshName("anon$"))
    q"""
      final class $clsName extends _root_.codes.quine.labo.dali.Generic[$tpe] {
        type Repr = $reprTpe
        def embed(a: $tpe): Repr = a match {case ..$cases}
        def eject(r: Repr): $tpe = {
          @_root_.scala.annotation.tailrec
          def unsafeGet(c: _root_.codes.quine.labo.dali.Coproduct): Any = c match {
            case _root_.codes.quine.labo.dali.Inl(h) => h
            case _root_.codes.quine.labo.dali.Inr(c) => unsafeGet(c)
          }
          unsafeGet(r).asInstanceOf[$tpe]
        }
      }
      new $clsName: _root_.codes.quine.labo.dali.Generic.Aux[$tpe, $reprTpe]
    """
  }
}
