package org.bertails

/*
 * the Visitor Pattern revisited
 * - gives the visitor's functions their own name
 * - pass all functions instead of one container holding them
 * - we assume that we are aware of the hierarchy
 *
 * remarks:
 * - some people would rename "accept" as "fold" and call that a catamorphism
 * - pattern matching vs type matching
 */
object VisitorPatternCatamorphism {

  case class Num(n: Int) extends Expr
  case class Sum(l: Expr, r: Expr) extends Expr
  case class Prod(l: Expr, r: Expr) extends Expr

  trait Expr {
    def accept[T](
      fnum: Num => T,
      fsum: Sum => T,
      fprod: Prod => T): T =
        this match {
          case num: Num => fnum(num)
          case sum: Sum => fsum(sum)
          case prod: Prod => fprod(prod)
        }
  }

  def eval(expr: Expr): Int = expr.accept(
    num => num.n,
    sum => eval(sum.l) + eval(sum.r),
    prod => eval(prod.l) * eval(prod.r))


  def main(args: Array[String]): Unit = {
    val expr = new Sum(new Num(1), new Prod(new Num(2), new Num(3)))
    assert(eval(expr) == 7)
  }

}
