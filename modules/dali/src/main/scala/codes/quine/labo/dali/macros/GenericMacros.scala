package codes.quine.labo
package dali
package macros

import scala.reflect.macros.whitebox

class GenericMacros(val c: whitebox.Context) {
  import c.universe._

  def abort(message: String): Nothing =
    c.abort(c.enclosingPosition, message)

  def type2classSymbol(t: Type): ClassSymbol = {
    val symbol = t.typeSymbol
    if (!symbol.isClass) {
      abort(s"$symbol is not a class or trait")
    }

    val classSymbol = symbol.asClass
    classSymbol.typeSignature
    classSymbol
  }

  def symbol2type(symbol: Symbol): Type = {
    if (!symbol.isType) {
      abort(s"$symbol is not type")
    }

    symbol.typeSignature
    symbol.asType.toType
  }

  val hlistType: Type = typeOf[HList]
  val coproductType: Type = typeOf[Coproduct]

  def isReprType(t: Type): Boolean =
    t <:< hlistType || t <:< coproductType

  def isProduct(t: Type): Boolean =
    t.typeSymbol.isClass && (type2classSymbol(t).isCaseClass || type2classSymbol(t).isModuleClass)

  def isCoproduct(t: Type): Boolean =
    t.typeSymbol.isClass && type2classSymbol(t).isSealed && type2classSymbol(t).knownDirectSubclasses
      .forall(isCoproductChild(t, _))

  def isCoproductChild(t: Type, childSymbol: Symbol): Boolean = {
    val parentTypeCons = t.typeConstructor

    val childTypeCons = symbol2type(childSymbol).typeConstructor
    if (childTypeCons.typeParams.size != parentTypeCons.typeParams.size) {
      return false
    }

    val parentSymbol = parentTypeCons.typeSymbol
    parentTypeCons.typeParams.zip(childTypeCons.typeParams).forall {
      case (parentParam, childParam) =>
        val parentParamType = symbol2type(parentParam)
        val childParamType = symbol2type(childParam)
        parentParamType.asSeenFrom(internal.thisType(childSymbol), parentSymbol) =:= childParamType
    }
  }

  def isHigherKind(typeCons: Type): Boolean =
    typeCons.typeParams.size == 1

  def fieldsOf(t: Type): List[(TermName, Type)] =
    t.decls.sorted.collect {
      case symbol: TermSymbol if symbol.isCaseAccessor && symbol.isGetter =>
        (symbol.name.toTermName, symbol.typeSignatureIn(t).finalResultType)
    }

  def hasParamType(t: Type, paramType: Type): Boolean =
    t =:= paramType || t.typeArgs.nonEmpty && t.typeArgs.forall(hasParamType(_, paramType))

  def replaceParamType(t: Type, paramType: Type, to: TypeName): Tree =
    if (t =:= paramType) tq"$to"
    else if (t.typeArgs.isEmpty) tq"${t.typeSymbol}"
    else tq"${t.typeSymbol}[..${t.typeArgs.map(replaceParamType(_, paramType, to))}]"

  def materialize[A: WeakTypeTag, R]: Tree = {
    val t = weakTypeOf[A]
    if (isReprType(t)) {
      abort("no Generic instance available for HList or Coproduct")
    }

    if (isProduct(t)) materializeProduct(t)
    else if (isCoproduct(t)) materializeCoproduct(t)
    else abort(s"no Generic instance is available for $t")
  }

  def materializeProduct(t: Type): Tree = {
    if (type2classSymbol(t).isModuleClass) {
      return materializeSingleton(t)
    }

    val symbol = t.typeSymbol
    val companion = symbol.companion

    val fields = fieldsOf(t)
    val elems = fields.map { case (name, t) => (TermName(c.freshName("pattern$")), t) }

    val cons = q"$companion(..${elems.map(_._1)})"
    val pattern = pq"$companion(..${elems.map { case (name, _) => pq"$name" }})"

    val reprCons = elems.foldRight(q"_root_.codes.quine.labo.dali.HNil": Tree) {
      case ((name, _), acc) => q"_root_.codes.quine.labo.dali.:*:($name, $acc)"
    }
    val reprPattern = elems.foldRight(pq"_root_.codes.quine.labo.dali.HNil": Tree) {
      case ((name, _), acc) => pq"_root_.codes.quine.labo.dali.:*:($name, $acc)"
    }
    val reprType = elems.foldRight(tq"_root_.codes.quine.labo.dali.HNil": Tree) {
      case ((_, t), acc) => tq"_root_.codes.quine.labo.dali.:*:[$t, $acc]"
    }

    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("Product$"))
    q"""
      final class $className extends _root_.codes.quine.labo.dali.Generic[$t] {
        type Repr = $reprType
        def embed($argName: $t): Repr = $argName match { case $pattern => $reprCons }
        def project($argName: Repr): $t = $argName match { case $reprPattern => $cons }
      }
      new $className: _root_.codes.quine.labo.dali.Generic.Aux[$t, $reprType]
    """
  }

  def materializeSingleton(t: Type): Tree = {
    val singleton = t match {
      case SingleType(_, symbol) => symbol
      case _                     => abort(s"BUG: $t is not singleton")
    }

    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("Singleton$"))
    q"""
      final class $className extends _root_.codes.quine.labo.dali.Generic[$t] {
        type Repr = _root_.codes.quine.labo.dali.HNil
        def embed($argName: $t): Repr = _root_.codes.quine.labo.dali.HNil
        def project($argName: Repr): $t = $singleton
      }
      new $className: _root_.codes.quine.labo.dali.Generic.Aux[$t, _root_.codes.quine.labo.dali.HNil]
    """
  }

  def materializeCoproduct(t: Type): Tree = {
    val children =
      t.typeSymbol.asClass.knownDirectSubclasses.toList
        .sortBy(_.fullName)
        .map(child => appliedType(child.asType.toType, t.typeArgs))

    val reprType = children.foldRight(tq"_root_.codes.quine.labo.dali.CNil": Tree) {
      case (t, acc) => tq"_root_.codes.quine.labo.dali.:+:[$t, $acc]"
    }
    val cases = children.zipWithIndex.map { case (t, i) => cq"_: $t => $i" }

    val argName = TermName(c.freshName("arg$"))
    val className = TypeName(c.freshName("Coproduct$"))
    q"""
      final class $className extends _root_.codes.quine.labo.dali.Generic[$t] {
        type Repr = $reprType
        def embed($argName: $t): Repr =
          _root_.codes.quine.labo.dali.Coproduct.unsafeApply($argName match { case ..$cases }, $argName).asInstanceOf[Repr]
        def project($argName: Repr): $t =
          _root_.codes.quine.labo.dali.Coproduct.unsafeGet($argName).asInstanceOf[$t]
      }
      new $className: _root_.codes.quine.labo.dali.Generic.Aux[$t, $reprType]
    """
  }

  def materialize1[F[_], R <: higher.TypeFunction1](implicit F: WeakTypeTag[F[_]]): Tree = {
    val typeCons = weakTypeOf[F[_]].typeConstructor
    if (!isHigherKind(typeCons)) {
      abort(s"no Generic1 instance is available for non * -> * kind type")
    }

    val param = typeCons.typeParams.head
    val paramType = param.asType.toType
    val typeA = appliedType(typeCons, paramType).dealias
    val typeCons0 = typeA.typeConstructor

    if (isProduct(typeCons0)) materializeProduct1(typeCons, paramType, typeA)
    else if (isCoproduct(typeCons0)) materializeCoproduct1(typeCons, paramType, typeA)
    else abort(s"no Generic1 instance is available for $typeCons")
  }

  def materializeProduct1(typeCons: Type, paramType: Type, typeA: Type): Tree = {
    val companion = typeA.typeSymbol.companion

    val fields = fieldsOf(typeA)
    val elems = fields.map { case (name, t) => (TermName(c.freshName("pattern$")), t) }

    val reprs = elems.map {
      case (name, t) if t =:= paramType => (name, tq"_root_.codes.quine.labo.dali.higher.Param1")
      case (name, t) if hasParamType(t, paramType) =>
        val lambdaTypeName = TypeName(c.freshName("Lambda$"))
        val newParamName = TypeName(c.freshName("A$"))
        val t1 = replaceParamType(t, paramType, newParamName)
        val lambda = tq"{type $lambdaTypeName[$newParamName] = $t1}"
        (name, tq"_root_.codes.quine.labo.dali.higher.Rec1[($lambda)#$lambdaTypeName]")
      case (name, t) => (name, tq"_root_.codes.quine.labo.dali.higher.Const1[$t]")
    }

    val cons = q"$companion(..${elems.map(_._1)})"
    val pattern = pq"$companion(..${elems.map { case (name, _) => pq"$name" }})"

    val reprCons = reprs.foldRight(q"_root_.codes.quine.labo.dali.HNil": Tree) {
      case ((name, _), acc) => q"_root_.codes.quine.labo.dali.:*:($name, $acc)"
    }
    val reprPattern = reprs.foldRight(pq"_root_.codes.quine.labo.dali.HNil": Tree) {
      case ((name, _), acc) => pq"_root_.codes.quine.labo.dali.:*:($name, $acc)"
    }
    val reprType = reprs.foldRight(tq"_root_.codes.quine.labo.dali.higher.HNil1": Tree) {
      case ((_, t), acc) => tq"_root_.codes.quine.labo.dali.higher.:**:[$t, $acc]"
    }

    val argName = TermName(c.freshName("a$"))
    val newParamName = TypeName(c.freshName("A$"))
    val newTypeA = replaceParamType(typeA, paramType, newParamName)

    val className = TypeName(c.freshName("Product1$"))
    q"""
      final class $className extends _root_.codes.quine.labo.dali.higher.Generic1[$typeCons] {
        type Repr1 = $reprType
        def embed[$newParamName]($argName: $newTypeA): Repr1#Apply[$newParamName] =
          $argName match { case $pattern => $reprCons }
        def project[$newParamName]($argName: Repr1#Apply[$newParamName]): $newTypeA =
          $argName match { case $reprPattern => $cons }
      }
      new $className: _root_.codes.quine.labo.dali.higher.Generic1.Aux[$typeCons, $reprType]
    """
  }

  def materializeCoproduct1(typeCons: Type, paramType: Type, typeA: Type): Tree = {
    val children =
      typeA.typeSymbol.asClass.knownDirectSubclasses.toList
        .sortBy(_.fullName)
        .map(t => appliedType(t.asType.toType, typeA.typeArgs))

    val reprs = children.map { t =>
      val lambdaTypeName = TypeName(c.freshName("Λ$"))
      val newParamName = TypeName(c.freshName("A$"))
      val t1 = replaceParamType(t, paramType, newParamName)
      tq"_root_.codes.quine.labo.dali.higher.Rec1[({type $lambdaTypeName[$newParamName] = $t1})#$lambdaTypeName]"
    }

    val argName = TermName(c.freshName("a$"))
    val newParamName = TypeName(c.freshName("A$"))
    val newTypeA = replaceParamType(typeA, paramType, newParamName)
    val newChildren = children.map(child => replaceParamType(child, paramType, newParamName))

    val reprType = reprs.foldRight(tq"_root_.codes.quine.labo.dali.higher.CNil1": Tree) {
      case (t, acc) => tq"_root_.codes.quine.labo.dali.higher.:++:[$t, $acc]"
    }
    val cases = newChildren.zipWithIndex.map { case (t, i) => cq"_: $t => $i" }

    val className = TypeName(c.freshName("Coproduct1$"))
    q"""
      final class $className extends _root_.codes.quine.labo.dali.higher.Generic1[$typeCons] {
        type Repr1 = $reprType
        def embed[$newParamName]($argName: $newTypeA): Repr1#Apply[$newParamName] =
          _root_.codes.quine.labo.dali.Coproduct.unsafeApply($argName match { case ..$cases }, $argName).asInstanceOf[Repr1#Apply[$newParamName]]
        def project[$newParamName]($argName: Repr1#Apply[$newParamName]): $newTypeA =
          _root_.codes.quine.labo.dali.Coproduct.unsafeGet($argName).asInstanceOf[$newTypeA]
      }
      new $className: _root_.codes.quine.labo.dali.higher.Generic1.Aux[$typeCons, $reprType]
    """
  }
}
