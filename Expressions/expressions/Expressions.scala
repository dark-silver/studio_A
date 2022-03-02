package expressions

/**
 * Playground for testing with expressions. Can be run as Scala application.
 */
object ExpressionTest extends App {
  import Expressions._

  // Exercise 1 in chapter 17.3.

  // Implement the expressions a, b, c and d.

  // Variables used in this exercise:
  def x = Variable("x")
  def y = Variable("y")
  def z = Variable("z")

  // a) 2x

  def func1 = ???

  // b) (2x)^3   eli  2x * 2x * 2x
  // Use the previous result here.

  def func2 = ???

  // c) 3 x y + x (x + 7 z)

  def func3 = ???

  // d) x^2 + 2 x y + y^2

  def func4 = ???

}

/**
 * Implementations for expression handling.
 */
object Expressions {

  // Exercise 2 in chapter 17.3.

  def prettyprint(e: Exp): String = {
    e match {
      case x: Const            => x.c.toString()
      case v: Variable         => v.name
      case Add(e: Exp, f: Exp) => "( " + prettyprint(e) + " + " + prettyprint(f) + " )"
      case Mul(e: Exp, f: Exp) => "( " + prettyprint(e) + " * " + prettyprint(f) + " )"
    }
  }

  // Exercise 3 in chapter 17.3.

  def bind(e: Exp, v: Variable, a: Double): Exp = {
    e match {
      case c: Const                  => ???
      case matchVar: Variable        => ???
      case add @ Add(e: Exp, f: Exp) => ???
      case mul @ Mul(e: Exp, f: Exp) => ???
    }
  }

  // Exercise 4 in chapter 17.3.

  def derivate(e: Exp, d: Variable): Exp = {
    e match {
      case Const(_)            => ???
      case v: Variable         => ???
      case Add(e: Exp, f: Exp) => ???
      case Mul(e: Exp, f: Exp) => ???
    }
  }

}