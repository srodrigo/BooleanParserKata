import org.scalatest.{FlatSpec, Matchers}
import BooleanCalculator.{evaluate, astAsString}

class BooleanCalculatorTest extends FlatSpec with Matchers {

    "T" should "be true" in {
        evaluate("T") should be(true)
    }

    "NOT T" should "be false" in {
        evaluate("NOT T") should be(false)
    }

    "T AND F" should "be false" in {
        evaluate("T AND F") should be(false)
    }

    "F OR T" should "be true" in {
        evaluate("F OR T") should be(true)
    }

    "NOT F AND T" should "be true" in {
        evaluate("NOT F AND T") should be(true)
    }

    "NOT T AND F AND T OR T" should "be true" in {
        evaluate("NOT T AND F AND T OR T") should be(true)
    }

    "NOT (T AND F AND T OR T)" should "be false" in {
        evaluate("NOT (T AND F AND T OR T)") should be (false)
    }

    "T AND (F AND T OR F)" should "be false" in {
        evaluate("T AND (F AND T OR F)") should be (false)
    }

    "NOT (T AND F AND T OR T)" should "print AST" in {
        println(astAsString("NOT (T AND F AND T OR T)"))
    }
}
