package codes.quine.labo
package dali

import minitest._

object CoproductSuite extends SimpleTestSuite {
  test("apply") {
    assert(Coproduct[Int :+: CNil](1).isInstanceOf[Int :+: CNil])
    assert(Coproduct[Int :+: String :+: CNil]("foo").isInstanceOf[Int :+: String :+: CNil])

    assertDoesNotCompile("""Coproduct[CNil](1)""")
    assertDoesNotCompile("""Coproduct[Int :+: CNil]("foo")""")
  }

  test("==") {
    assert(Coproduct[Int :+: CNil](1) == Inl(1))
    assert(Coproduct[Int :+: String :+: CNil]("foo") == Inr(Inl("foo")))
  }

  test("toString") {
    assertEquals(Coproduct[Int :+: CNil](1).toString, "Inl(1)")
    assertEquals(Coproduct[Int :+: String :+: CNil]("foo").toString, "Inr(Inl(foo))")
  }

  test("unapply") {
    assert(Coproduct[Int :+: CNil](1) match {
      case Inl(i) => i == 1
      case _      => false
    })

    assert(Coproduct[Int :+: String :+: CNil]("foo") match {
      case Inr(Inl(s)) => s == "foo"
      case _           => false
    })
  }

  test("unsafeApply") {
    assert(Coproduct.unsafeApply(0, 1).asInstanceOf[Int :+: CNil] == Inl(1))
    assert(Coproduct.unsafeApply(1, 1).asInstanceOf[String :+: Int :+: CNil] == Inr(Inl(1)))
  }
}
