package codes.quine.labo
package dali

sealed trait Foo

sealed trait Bar extends Foo
case class Bar2(x: Int) extends Bar
case class Baz(x: Int, y: String) extends Foo
case object Fizz extends Foo
