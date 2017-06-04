import org.scalatest.{FlatSpec, Matchers}
import BooleanCalculator.evaluate

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
}
