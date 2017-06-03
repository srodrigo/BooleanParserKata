import org.scalatest.{FlatSpec, Matchers}
import BooleanCalculator.evaluate

class BooleanCalculatorTest extends FlatSpec with Matchers {

    "T" should "be true" in {
        evaluate("T") should be(true)
    }

    "F" should "be false" in {
        evaluate("F") should be(false)
    }
}
