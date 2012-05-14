package org.bertails

/**
 * the Visitor Pattern with no generics
 * the visitor needs to carry a state
 */
object VisitorPatternState {

  trait ExprVisitor {
    def visit(num: Num): Unit
    def visit(sum: Sum): Unit
    def visit(prod: Prod): Unit
  }

  trait Expr {
    def accept(visitor: ExprVisitor): Unit
  }

  case class Num(n: Int) extends Expr {
    def accept(visitor: ExprVisitor): Unit = visitor.visit(this)
  }

  case class Sum(l: Expr, r: Expr) extends Expr {
    def accept(visitor: ExprVisitor): Unit = visitor.visit(this)
  }

  case class Prod(l: Expr, r: Expr) extends Expr {
    def accept(visitor: ExprVisitor): Unit = visitor.visit(this)
  }

  class Eval extends ExprVisitor {
    var result: Int = 0
    def visit(num: Num): Unit = {
      result = num.n
    }
    def visit(sum: Sum): Unit = {
      sum.l.accept(this)
      val lresult = result
      sum.r.accept(this)
      val rresult = result
      result = lresult + rresult
    }
    def visit(prod: Prod): Unit = {
      prod.l.accept(this)
      val lresult = result
      prod.r.accept(this)
      val rresult = result
      result = lresult * rresult
    }
  }

  def eval(expr: Expr): Int = {
    val evaluator = new Eval
    expr.accept(evaluator)
    evaluator.result
  }

  def main(args: Array[String]): Unit = {
    val expr = new Sum(new Num(1), new Prod(new Num(2), new Num(3)))
    assert(eval(expr) == 7)
  }

}
