package codes.quine.labo
package dali

import minitest._

object HListSuite extends SimpleTestSuite {
  test("HNil") {
    assert(HNil.isInstanceOf[HNil])
  }

  test(":*:") {
    assert((1 :*: HNil).isInstanceOf[Int :*: HNil])
  }

  test("==") {
    assert(HNil == HNil)
    assert((1 :*: HNil) == (1 :*: HNil))
  }

  test("toString") {
    assertEquals(HNil.toString, "HNil")
    assertEquals((1 :*: HNil).toString, "1 :*: HNil")
  }

  test("unapply") {
    assert(HNil match { case HNil => true })

    assert((1 :*: HNil) match { case i :*: HNil => i == 1 })
  }
}
