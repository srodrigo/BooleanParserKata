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

    "NOT F" should "be Boolean Value T" in {
        BooleanParser(List(NEGATION_OP, BOOLEAN_VAL(false))).right.get should be(BooleanValue(true))
    }

    "T AND T" should "be Boolean Expr T" in {
        BooleanParser(List(BOOLEAN_VAL(true), AND_OP, BOOLEAN_VAL(false))).right.get should be(AndOp(BooleanValue(true), BooleanValue(false)))
    }
}
