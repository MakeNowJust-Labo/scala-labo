package codes.quine.labo
package dali

import minitest._

object GenericSuite extends SimpleTestSuite {
  import higher.Generic1Suite.{MyCons, MyList, MyNil}

  sealed trait Expr
  object Zero extends Expr
  case class Val(x: Int) extends Expr
  case class Add(l: Expr, r: Expr) extends Expr

  def assertGeneric[A: Generic](a: A): Unit =
    assertEquals(Generic[A].project(Generic[A].embed(a)), a)

  test("Expr") {
    val v1 = Val(1)
    val v2 = Val(2)
    val add = Add(v1, v2)

    Generic[Expr]: Generic.Aux[Expr, Add :+: Val :+: Zero.type :+: CNil]
    Generic[Zero.type]: Generic.Aux[Zero.type, HNil]
    Generic[Val]: Generic.Aux[Val, Int :*: HNil]
    Generic[Add]: Generic.Aux[Add, Expr :*: Expr :*: HNil]

    assertGeneric[Expr](Zero)
    assertGeneric[Expr](v1)
    assertGeneric[Expr](v2)
    assertGeneric[Expr](add)
    assertGeneric(Zero)
    assertGeneric(v1)
    assertGeneric(v2)
    assertGeneric(add)
  }

  test("MyList") {
    val nil = MyNil[Int]
    val cons = MyCons(1, nil)
    val listCons = MyCons(List(1, 2), MyNil[List[Int]])

    Generic[MyList[Int]]: Generic.Aux[MyList[Int], MyCons[Int] :+: MyNil[Int] :+: CNil]
    Generic[MyNil[Int]]: Generic.Aux[MyNil[Int], HNil]
    Generic[MyCons[Int]]: Generic.Aux[MyCons[Int], Int :*: MyList[Int] :*: HNil]

    assertGeneric[MyList[Int]](nil)
    assertGeneric[MyList[Int]](cons)
    assertGeneric(nil)
    assertGeneric(cons)

    assertGeneric[MyList[List[Int]]](listCons)
    assertGeneric(listCons)
  }

  test("TupleN") {
    assertGeneric(())
    assertGeneric((1, "foo"))
    assertGeneric((1, "foo", 3))
  }
}
