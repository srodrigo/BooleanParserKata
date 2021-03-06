import org.scalatest.{FlatSpec, Matchers}

class BooleanParserTest extends FlatSpec with Matchers {

    "T" should "be Boolean Value T" in {
        BooleanParser(List(TRUE_VAL)).right.get should be(TrueValue)
    }

    "F" should "be Boolean Value F" in {
        BooleanParser(List(FALSE_VAL)).right.get should be(FalseValue)
    }

    "NOT T" should "be Not Op T Expr" in {
        BooleanParser(
            List(
                NOT_OP,
                TRUE_VAL)).right.get should be(
            NotOp(TrueValue))
    }

    "NOT F" should "be Not Op F Expr" in {
        BooleanParser(
            List(
                NOT_OP,
                FALSE_VAL)).right.get should be(
            NotOp(FalseValue))
    }

    "T AND F" should "be And Op Expr" in {
        BooleanParser(
            List(
                TRUE_VAL,
                AND_OP,
                FALSE_VAL)).right.get should be(
            AndOp(
                TrueValue,
                FalseValue))
    }

    "F OR T" should "be OR Op Expr" in {
        BooleanParser(
            List(
                FALSE_VAL,
                OR_OP,
                TRUE_VAL)).right.get should be(
            OrOp(
                FalseValue,
                TrueValue))
    }

    "NOT F AND T" should "be And Op Expr" in {
        BooleanParser(
            List(
                NOT_OP,
                FALSE_VAL,
                AND_OP,
                TRUE_VAL)).right.get should be(
            AndOp(
                NotOp(FalseValue),
                TrueValue))
    }

    "F AND T OR T" should "be Or, And Expr" in {
        BooleanParser(
            List(
                FALSE_VAL,
                AND_OP,
                TRUE_VAL,
                OR_OP,
                TRUE_VAL)).right.get should be(
            OrOp(
                AndOp(
                    FalseValue,
                    TrueValue),
                TrueValue))
    }

    "F OR T AND T" should "be Or, And Expr" in {
        BooleanParser(
            List(
                FALSE_VAL,
                OR_OP,
                TRUE_VAL,
                AND_OP,
                TRUE_VAL)).right.get should be(
            OrOp(
                FalseValue,
                AndOp(
                    TrueValue,
                    TrueValue)))
    }

    "T AND F AND T" should "be And, And Expr" in {
        BooleanParser(
            List(
                TRUE_VAL,
                AND_OP,
                FALSE_VAL,
                AND_OP,
                TRUE_VAL)).right.get should be(
            AndOp(
                TrueValue,
                AndOp(FalseValue,
                    TrueValue)))
    }

    "T OR F OR T" should "be Or, Or Expr" in {
        BooleanParser(
            List(
                TRUE_VAL,
                OR_OP,
                FALSE_VAL,
                OR_OP,
                TRUE_VAL)).right.get should be(
            OrOp(
                TrueValue,
                OrOp(FalseValue,
                    TrueValue)))
    }

    "T OR F OR T AND F AND T OR T" should "be Or, Or, And, And, Or Expr" in {
        BooleanParser(
            List(
                TRUE_VAL,
                OR_OP,
                FALSE_VAL,
                OR_OP,
                TRUE_VAL,
                AND_OP,
                FALSE_VAL,
                AND_OP,
                TRUE_VAL,
                OR_OP,
                TRUE_VAL)).right.get should be(
            OrOp(
                TrueValue,
                OrOp(
                    FalseValue,
                    OrOp(
                        AndOp(
                            TrueValue,
                            AndOp(
                                FalseValue,
                                TrueValue)),
                        TrueValue))))
    }

    "NOT (T AND F)" should "be True" in {
        BooleanParser(
            List(
                NOT_OP,
                OPEN_PAR,
                TRUE_VAL,
                AND_OP,
                FALSE_VAL,
                CLOSE_PAR)).right.get should be(
            NotOp(
                AndOp(
                    TrueValue,
                    FalseValue)))
    }

    "NOT (T AND F OR T)" should "be False" in {
        BooleanParser(
            List(
                NOT_OP,
                OPEN_PAR,
                TRUE_VAL,
                AND_OP,
                FALSE_VAL,
                OR_OP,
                TRUE_VAL,
                CLOSE_PAR)).right.get should be(
            NotOp(
                OrOp(
                    AndOp(
                        TrueValue,
                        FalseValue),
                    TrueValue)))
    }

    "T AND (F AND T OR F)" should "be Not, Or, And" in {
        BooleanParser(
            List(
                TRUE_VAL,
                AND_OP,
                OPEN_PAR,
                FALSE_VAL,
                AND_OP,
                TRUE_VAL,
                OR_OP,
                FALSE_VAL,
                CLOSE_PAR)).right.get should be(
            AndOp(
                TrueValue,
                OrOp(
                    AndOp(
                        FalseValue,
                        TrueValue),
                    FalseValue)))
    }
}
