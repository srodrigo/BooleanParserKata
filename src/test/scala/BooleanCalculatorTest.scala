import org.scalatest.{FlatSpec, Matchers}
import BooleanCalculator.{evaluate, astAsString}

class BooleanCalculatorTest extends FlatSpec with Matchers {

    "NOT T AND F AND T OR T" should "be true" in {
        evaluate("NOT T AND F AND T OR T") should be(true)
    }

    "NOT (T AND F AND T OR T)" should "be false" in {
        evaluate("NOT (T AND F AND T OR T)") should be (false)
    }

    "T AND (F AND T OR F)" should "be false" in {
        evaluate("T AND (F AND T OR F)") should be (false)
    }

    "NOT (T AND F AND T OR T)" should "format AST as string" in {
        astAsString("NOT (T AND F AND T OR T)") should be ("NOT (OR (AND (AND (T, F), T), T))")
    }

    "T OR F OR T AND F AND T OR T" should "format AST as string" in {
        astAsString("T OR F OR T AND F AND T OR T") should be ("OR (T, OR (F, OR (AND (T, AND (F, T)), T)))")
    }
}
