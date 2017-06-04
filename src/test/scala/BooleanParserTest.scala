import BooleanCalculator.evaluate
import org.scalatest.{FlatSpec, Matchers}

class BooleanParserTest extends FlatSpec with Matchers {

    "T" should "be Boolean Value T" in {
        BooleanParser(List(BOOLEAN_VAL("T"))).right.get should be(BooleanValue("T"))
    }

    "F" should "be Boolean Value F" in {
        BooleanParser(List(BOOLEAN_VAL("F"))).right.get should be(BooleanValue("F"))
    }

    "NOT T" should "be Boolean Value F" in {
        BooleanParser(List(NEGATION_OP, BOOLEAN_VAL("F"))).right.get should be(BooleanValue("F"))
    }
}
