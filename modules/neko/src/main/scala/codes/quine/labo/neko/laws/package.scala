package codes.quine.labo
package neko

package object laws {
  implicit final class ToIsEqOps[A](private val lhs: A) extends AnyVal {
    def <->(rhs: A): IsEq[A] = IsEq(lhs, rhs)

    def ==>(rhs: Boolean)(implicit ev: A =:= Boolean): IsEq[Boolean] = (!ev(lhs) || rhs) <-> true
  }
}
