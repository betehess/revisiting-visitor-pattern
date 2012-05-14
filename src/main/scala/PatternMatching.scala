package org.bertails

object patternmatching {

  sealed trait Expr
  case class Num(n: Int) extends Expr
  case class Sum(l: Expr, r: Expr) extends Expr
  case class Prod(l: Expr, r: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Num(n) => n
    case Sum(l, r) => eval(l) + eval(r)
    case Prod(l, r) => eval(l) * eval(r)
  }

  def main(args: Array[String]): Unit = {
    val expr = new Sum(new Num(1), new Prod(new Num(2), new Num(3)))
    assert(eval(expr) == 7)
  }

}
