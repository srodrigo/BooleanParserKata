import org.scalatest.{FlatSpec, Matchers}

class BooleanParserTest extends FlatSpec with Matchers {

    "T" should "be Boolean Value T" in {
        BooleanParser(List(BOOLEAN_VAL(true))).right.get should be(BooleanValue(true))
    }

    "F" should "be Boolean Value F" in {
        BooleanParser(List(BOOLEAN_VAL(false))).right.get should be(BooleanValue(false))
    }

    "NOT T" should "be Not Op T Expr" in {
        BooleanParser(List(NEGATION_OP, BOOLEAN_VAL(true))).right.get should be(NotOp(BooleanValue(true)))
    }

    "NOT F" should "be Not Op F Expr" in {
        BooleanParser(List(NEGATION_OP, BOOLEAN_VAL(false))).right.get should be(NotOp(BooleanValue(false)))
    }

    "T AND F" should "be And Op Expr" in {
        BooleanParser(List(BOOLEAN_VAL(true), AND_OP, BOOLEAN_VAL(false))).right.get should be(
            AndOp(
                BooleanValue(true),
                BooleanValue(false)))
    }

    "F OR T" should "be OR Op Expr" in {
        BooleanParser(List(BOOLEAN_VAL(false), OR_OP, BOOLEAN_VAL(true))).right.get should be(
            OrOp(
                BooleanValue(false),
                BooleanValue(true)))
    }

    "NOT F AND T" should "be And Op Expr" in {
        BooleanParser(
            List(
                NEGATION_OP,
                BOOLEAN_VAL(false),
                AND_OP,
                BOOLEAN_VAL(true))).right.get should be(
            AndOp(
                NotOp(BooleanValue(false)),
                BooleanValue(true)))
    }
}
