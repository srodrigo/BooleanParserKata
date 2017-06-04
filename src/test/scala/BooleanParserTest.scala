import org.scalatest.{FlatSpec, Matchers}

class BooleanParserTest extends FlatSpec with Matchers {

    "T" should "be Boolean Value T" in {
        BooleanParser(List(BOOLEAN_VAL(true))).right.get should be(BooleanValue(true))
    }

    "F" should "be Boolean Value F" in {
        BooleanParser(List(BOOLEAN_VAL(false))).right.get should be(BooleanValue(false))
    }

    "NOT T" should "be Boolean Value F" in {
        BooleanParser(List(NEGATION_OP, BOOLEAN_VAL(true))).right.get should be(BooleanValue(false))
    }
}
