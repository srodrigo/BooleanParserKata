object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean = {
        BooleanLexer(booleanExpr).right.get match {
            case BOOLEAN_VAL(booleanVal) =>
                if (booleanVal.equals("T")) true
                else false
        }
    }
}
