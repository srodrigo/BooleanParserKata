object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean = {
        val ast: Either[Object, BooleanAST] = for {
            token <- BooleanLexer(booleanExpr).right
            ast <- BooleanParser(token).right
        } yield ast

        eval(ast.right.get)
    }

    private def eval(ast: BooleanAST): Boolean = ast match {
        case BooleanValue(value) =>
            if (value == "T") true
            else false
    }
}
