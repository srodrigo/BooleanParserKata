object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean = {
        val ast: Either[Object, BooleanAST] = for {
            tokens <- BooleanLexer(booleanExpr).right
            ast <- BooleanParser(tokens).right
        } yield ast

        eval(ast.right.get)
    }

    private def eval(ast: BooleanAST): Boolean = ast match {
        case BooleanValue(bool) => bool
        case NotOp(BooleanValue(bool)) => !bool
        case AndOp(BooleanValue(left), BooleanValue(right)) => left && right
        case OrOp(BooleanValue(left), BooleanValue(right)) => left || right
    }
}
