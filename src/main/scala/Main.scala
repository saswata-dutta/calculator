import Token._

import scala.collection.mutable

object Main {

  def main(args: Array[String]): Unit = {
    val input = args(0)
    val infix = tokenise(input)
    val postfix = toPostfix(infix)

    println(eval(postfix))
  }

  def tokenise(in: String): Seq[Token] =
    in.split("""\s+""")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(Token.create)

  def toPostfix(infix: Seq[Token]): Seq[Token] = {

    val postfix = mutable.Buffer[Token]()
    val opStack = mutable.Stack[Op]()

    infix.foreach {
      case v: Num => postfix += v
      case op: Op =>
        if (opStack.isEmpty || opStack.top == OpenParen || op == OpenParen) {

          opStack.push(op)
        } else if (op == CloseParen) {

          var found = false
          while (opStack.nonEmpty && !found) {
            val top = opStack.pop
            if (top == OpenParen) found = true
            else postfix += top
          }
          if (!found) throw new IllegalArgumentException("Unbalanced Close Paren")
        } else {

          // todo
          // If the incoming symbol has equal precedence with the top of the stack, use association.
          // If the association is left to right, pop and print the top of the stack and then push the incoming operator.
          // If the association is right to left, push the incoming operator.
          if (cmpPrecedence(op, opStack.top) <= 0) {

            while (opStack.nonEmpty && cmpPrecedence(op, opStack.top) <= 0) {
              postfix += opStack.pop
            }
          }

          opStack.push(op)
        }
    }

    while (opStack.nonEmpty) {
      if (opStack.top == OpenParen)
        throw new IllegalArgumentException("Unbalanced Open Paren")
      postfix += opStack.pop
    }

    postfix
  }

  def eval(postfix: Seq[Token]): Double = {
    val numStack = mutable.Stack[Double]()
    postfix.foreach {
      case Num(value) => numStack.push(value)
      case op: Op =>
        val rhs = numStack.pop()
        val lhs = numStack.pop()
        numStack.push(eval(lhs, rhs, op))
    }

    require(numStack.size == 1, "Bad postfix")
    numStack.pop()
  }

  def eval(lhs: Double, rhs: Double, op: Op): Double = op match {
    case Div => lhs / rhs
    case Mul => lhs * rhs
    case Add => lhs + rhs
    case Sub => lhs - rhs
    case _   => throw new IllegalArgumentException(s"Bad Operator Eval ${op.symbol}")
  }
}