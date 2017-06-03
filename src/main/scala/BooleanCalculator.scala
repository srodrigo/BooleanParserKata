object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean = {
        val token = BooleanLexer(booleanExpr).right.get
        BooleanParser(token)
    }
}
