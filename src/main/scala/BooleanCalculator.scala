object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean = {
        val ast: Either[Object, BooleanAST] = for {
            tokens <- BooleanLexer(booleanExpr).right
            ast <- BooleanParser(tokens).right
        } yield ast

        eval(ast.right.get)
    }

    private def eval(ast: BooleanAST): Boolean = ast match {
        case TrueValue => true
        case FalseValue => false
        case NotOp(expr) => !eval(expr)
        case AndOp(left, right) => eval(left) && eval(right)
        case OrOp(left, right) => eval(left) || eval(right)
    }
}
