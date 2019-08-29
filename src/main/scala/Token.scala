import java.io.Serializable

sealed trait Token extends Product with Serializable

final case class Num(value: Double) extends Token
final case class Op(symbol: Char) extends Token

object Token {
  val Add: Op = Op('+')
  val Sub: Op = Op('-')
  val Mul: Op = Op('*')
  val Div: Op = Op('/')
  val OpenParen: Op = Op('(')
  val CloseParen: Op = Op(')')

  val operators: Map[String, Token] =
    Map("+" -> Add, "-" -> Sub, "*" -> Mul, "/" -> Div, "(" -> OpenParen, ")" -> CloseParen)

  def create(snip: String): Token =
    operators.getOrElse(snip, Num(snip.toDouble))

  // assume all operators left associative
  val operatorPrecedence: Map[Op, Int] = Map(Mul -> 2, Div -> 2, Add -> 1, Sub -> 1)

  def cmpPrecedence(lhs: Op, rhs: Op): Int =
    operatorPrecedence(lhs) - operatorPrecedence(rhs)
}
