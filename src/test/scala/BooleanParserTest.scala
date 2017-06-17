import org.scalatest.{FlatSpec, Matchers}

class BooleanParserTest extends FlatSpec with Matchers {

    "T" should "be Boolean Value T" in {
        BooleanParser(List(TRUE_VAL)).right.get should be(TrueValue)
    }

    "F" should "be Boolean Value F" in {
        BooleanParser(List(FALSE_VAL)).right.get should be(FalseValue)
    }

    "NOT T" should "be Not Op T Expr" in {
        BooleanParser(List(NEGATION_OP, TRUE_VAL)).right.get should be(NotOp(TrueValue))
    }

    "NOT F" should "be Not Op F Expr" in {
        BooleanParser(List(NEGATION_OP, FALSE_VAL)).right.get should be(NotOp(FalseValue))
    }

    "T AND F" should "be And Op Expr" in {
        BooleanParser(List(TRUE_VAL, AND_OP, FALSE_VAL)).right.get should be(
            AndOp(
                TrueValue,
                FalseValue))
    }

    "F OR T" should "be OR Op Expr" in {
        BooleanParser(List(FALSE_VAL, OR_OP, TRUE_VAL)).right.get should be(
            OrOp(
                FalseValue,
                TrueValue))
    }

    "NOT F AND T" should "be And Op Expr" in {
        BooleanParser(
            List(
                NEGATION_OP,
                FALSE_VAL,
                AND_OP,
                TRUE_VAL)).right.get should be(
            NotOp(
                AndOp(
                    FalseValue,
                    TrueValue)))
    }

    "F AND T OR T" should "be And, Or Expr" in {
        BooleanParser(
            List(
                FALSE_VAL,
                AND_OP,
                TRUE_VAL,
                OR_OP,
                TRUE_VAL)).right.get should be(
            AndOp(
                FalseValue,
                OrOp(
                    TrueValue,
                    TrueValue)))
    }
}
