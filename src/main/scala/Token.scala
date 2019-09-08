import java.io.Serializable

sealed trait Token extends Product with Serializable

final case class Num(value: Double) extends Token
sealed trait Op extends Token
case object Add extends Op
case object Sub extends Op
case object Mul extends Op
case object Div extends Op
case object OpenParen extends Op
case object CloseParen extends Op

object Token {

  val operatorsSymbols: Map[String, Token] =
    Map("+" -> Add, "-" -> Sub, "*" -> Mul, "/" -> Div, "(" -> OpenParen, ")" -> CloseParen)

  def create(snip: String): Token =
    if (snip.length > 1 || snip.head.isDigit) Num(snip.toDouble)
    else operatorsSymbols(snip)

  // assume all operators left associative
  val operatorPrecedence: Map[Op, Int] = Seq(Div, Mul, Add, Sub).zipWithIndex.toMap

  def cmpPrecedence(lhs: Op, rhs: Op): Int =
    operatorPrecedence(lhs) - operatorPrecedence(rhs)
}
