package org.bertails

/*
 * the Visitor Pattern with generics
 * the Visitor's type carries the kind of value that is computed
 */
object VisitorPatternGenerics {

  trait ExprVisitor[T] {
    def visit(num: Num): T
    def visit(sum: Sum): T
    def visit(prod: Prod): T
  }

  trait Expr {
    def accept[T](visitor: ExprVisitor[T]): T
  }

  case class Num(n: Int) extends Expr {
    def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)
  }

  case class Sum(l: Expr, r: Expr) extends Expr {
    def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)
  }

  case class Prod(l: Expr, r: Expr) extends Expr {
    def accept[T](visitor: ExprVisitor[T]): T = visitor.visit(this)
  }

  class Eval extends ExprVisitor[Int] {
    def visit(num: Num): Int = num.n
    def visit(sum: Sum): Int = sum.l.accept(this) + sum.r.accept(this)
    def visit(prod: Prod): Int = prod.l.accept(this) * prod.r.accept(this)
  }

  def eval(expr: Expr): Int = expr.accept(new Eval)

  def main(args: Array[String]): Unit = {
    val expr = new Sum(new Num(1), new Prod(new Num(2), new Num(3)))
    assert(eval(expr) == 7)
  }

}
