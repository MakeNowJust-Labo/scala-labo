package codes.quine.labo
package dali
package higher

import minitest._

object Generic1Suite extends SimpleTestSuite {
  sealed trait MyList[A]
  case class MyNil[A]() extends MyList[A]
  case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

  sealed trait MyEither[A, B]
  case class MyLeft[A, B](left: A) extends MyEither[A, B]
  case class MyRight[A, B](right: B) extends MyEither[A, B]

  def assertGeneric1[F[_]: Generic1, A](fa: F[A]): Unit =
    assertEquals(Generic1[F].project(Generic1[F].embed(fa)), fa)

  test("MyList") {
    val nil = MyNil[Int]
    val cons = MyCons(1, nil)

    Generic1[MyList]: Generic1.Aux[MyList, Rec1[MyCons] :++: Rec1[MyNil] :++: CNil1]
    Generic1[MyNil]: Generic1.Aux[MyNil, HNil1]
    Generic1[MyCons]: Generic1.Aux[MyCons, Param1 :**: Rec1[MyList] :**: HNil1]

    assertGeneric1[MyList, Int](nil)
    assertGeneric1[MyList, Int](cons)
    assertGeneric1(nil)
    assertGeneric1(cons)

    assertEquals(Functor[MyList].map(cons)(_ + 1), MyCons(2, MyNil[Int]))
  }

  test("MyEither") {
    val left = MyLeft[String, Int]("foo")
    val right = MyRight[String, Int](1)

    Generic1[MyEither[String, *]]: Generic1.Aux[MyEither[String, *], Rec1[MyLeft[String, *]] :++: Rec1[
      MyRight[String, *]
    ] :++: CNil1]
    Generic1[MyLeft[String, *]]: Generic1.Aux[MyLeft[String, *], Const1[String] :**: HNil1]
    Generic1[MyRight[String, *]]: Generic1.Aux[MyRight[String, *], Param1 :**: HNil1]

    assertGeneric1[MyEither[String, *], Int](left)
    assertGeneric1[MyEither[String, *], Int](right)
    assertGeneric1(left)
    assertGeneric1(right)

    assertEquals(Functor[MyEither[String, *]].map(left)(_ + 1), left)
    assertEquals(Functor[MyEither[String, *]].map(right)(_ + 1), MyRight(2))
  }
}
