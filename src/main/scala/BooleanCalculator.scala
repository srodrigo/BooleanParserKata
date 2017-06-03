object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean = {
        val ast: Either[Object, BooleanAST] = for {
            tokens <- BooleanLexer(booleanExpr).right
            ast <- BooleanParser(tokens).right
        } yield ast

        eval(ast.right.get)
    }

    private def eval(ast: BooleanAST): Boolean = ast match {
        case BooleanValue(value) =>
            if (value == "T") true
            else false
    }
}
