import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object BooleanParser extends Parsers {
    override type Elem = Token

    def apply(tokens: List[Token]): Either[String, BooleanAst] = {
        val reader = new BooleanTokenReader(tokens)
        expression(reader) match {
            case NoSuccess(msg, _) => Left(msg)
            case Success(result, _) => Right(result)
        }
    }

    private def expression: Parser[BooleanAst] = phrase(block)

    private def block: Parser[BooleanAst] =
        rep1(booleanExpr) ^^ (exprList => exprList reduceRight OrOp)

    private def booleanExpr: Parser[BooleanAst] = orOp | andOp | andOpPar | booleanValue
    private def term: Parser[BooleanAst] = andOp | andOpPar | booleanValue
    private def booleanValue: Parser[BooleanAst] = trueValue | falseValue | notOp | notOpPar
    private def trueValue: Parser[BooleanAst] = TRUE_VAL ^^^ TrueValue
    private def falseValue: Parser[BooleanAst] = FALSE_VAL ^^^ FalseValue

    private val notOp = NOT_OP ~ booleanValue ^^ {
        case _ ~ booleanValue => NotOp(booleanValue)
    }

    private val notOpPar = NOT_OP ~ OPEN_PAR ~ booleanExpr ~ CLOSE_PAR ^^ {
        case _ ~ _ ~ expr ~ _ => NotOp(expr)
    }

    private val andOp = booleanValue ~ AND_OP ~ term ^^ {
        case left ~ _ ~ right => AndOp(left, right)
    }

    private val andOpPar = booleanValue ~ AND_OP ~ OPEN_PAR ~ booleanExpr ~ CLOSE_PAR ^^ {
        case left ~ _ ~ _ ~ expr ~ _ => AndOp(left, expr)
    }

    private val orOp = term ~ OR_OP ~ booleanExpr ^^ {
        case left ~ _ ~ right => OrOp(left, right)
    }
}

sealed trait BooleanAst

final case class AndOp(left: BooleanAst, right: BooleanAst) extends BooleanAst
final case class OrOp(left: BooleanAst, right: BooleanAst) extends BooleanAst
final case class NotOp(expr: BooleanAst) extends BooleanAst
final case class Parenthesis(expr: BooleanAst) extends BooleanAst

sealed trait BooleanValue extends BooleanAst
case object TrueValue extends BooleanValue
case object FalseValue extends BooleanValue

class BooleanTokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new BooleanTokenReader(tokens.tail)
}
