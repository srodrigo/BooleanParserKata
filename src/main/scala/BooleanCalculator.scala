object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean =
        evaluateAST(buildAST(booleanExpr))

    def astAsString(booleanExpr: String): String =
        formatAST(buildAST(booleanExpr))

    private def buildAST(booleanExpr: String) = {
        val ast: Either[Object, BooleanAST] = for {
            tokens <- BooleanLexer(booleanExpr).right
            ast <- BooleanParser(tokens).right
        } yield ast

        ast.right.get
    }

    private def evaluateAST(ast: BooleanAST): Boolean = ast match {
        case TrueValue => true
        case FalseValue => false
        case NotOp(expr) => !evaluateAST(expr)
        case AndOp(left, right) => evaluateAST(left) && evaluateAST(right)
        case OrOp(left, right) => evaluateAST(left) || evaluateAST(right)
    }

    def formatAST(ast: BooleanAST): String = ast match {
        case TrueValue => "T"
        case FalseValue => "F"
        case NotOp(expr) => "NOT - " + formatAST(expr)
        case AndOp(left, right) => "AND - " + formatAST(left) + " | " + formatAST(right)
        case OrOp(left, right) => "OR - " + formatAST(left) + " | " + formatAST(right)
    }
}
