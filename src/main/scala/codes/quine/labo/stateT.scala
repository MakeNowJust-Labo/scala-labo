package codes.quine.labo

object nekoStateT {
  import neko._, data._, instances._, syntax._

  val fa = StateT[Option, Boolean, Boolean] {
    case true  => None
    case false => Some((true, false))
  }

  val ff = StateT[Option, Boolean, Boolean => Boolean] {
    case true  => Some((true, x => !x))
    case false => Some((true, x => x))
  }

  val fg = StateT[Option, Boolean, Boolean => Boolean] {
    case true  => Some((false, x => !x))
    case false => Some((true, x => !x))
  }

  def test(): Unit = {
    val x = (ff <+> fg) <*> fa
    val y = (ff <*> fa) <+> (fg <*> fa)

    println(s"((ff <+> fg) <+> fa)         .run(true)  ==> ${x.run(true)}")
    println(s"((ff <+> fg) <+> fa)         .run(false) ==> ${x.run(false)}")
    println(s"((ff <*> fa) <+> (fg <*> fa)).run(true)  ==> ${y.run(true)}")
    println(s"((ff <*> fa) <+> (fg <*> fa)).run(false) ==> ${y.run(false)}")
    // Result:
    // ((ff <+> fg) <+> fa)         .run(true)  ==> None
    // ((ff <+> fg) <+> fa)         .run(false) ==> None
    // ((ff <*> fa) <+> (fg <*> fa)).run(true)  ==> Some((true,true))
    // ((ff <*> fa) <+> (fg <*> fa)).run(false) ==> None
  }
}

object catsStateT {
  import cats._, data._, implicits._

  val fa = StateT[Option, Boolean, Boolean] {
    case true  => None
    case false => Some((true, false))
  }

  val ff = StateT[Option, Boolean, Boolean => Boolean] {
    case true  => Some((true, x => !x))
    case false => Some((true, x => x))
  }

  val fg = StateT[Option, Boolean, Boolean => Boolean] {
    case true  => Some((false, x => !x))
    case false => Some((true, x => !x))
  }

  def test(): Unit = {
    val x = (ff <+> fg) <*> fa
    val y = (ff <*> fa) <+> (fg <*> fa)

    println(s"((ff <+> fg) <+> fa)         .run(true)  ==> ${x.run(true)}")
    println(s"((ff <+> fg) <+> fa)         .run(false) ==> ${x.run(false)}")
    println(s"((ff <*> fa) <+> (fg <*> fa)).run(true)  ==> ${y.run(true)}")
    println(s"((ff <*> fa) <+> (fg <*> fa)).run(false) ==> ${y.run(false)}")
  }
  // Result:
  // ((ff <+> fg) <+> fa)         .run(true)  ==> None
  // ((ff <+> fg) <+> fa)         .run(false) ==> None
  // ((ff <*> fa) <+> (fg <*> fa)).run(true)  ==> Some((true,true))
  // ((ff <*> fa) <+> (fg <*> fa)).run(false) ==> None
}
