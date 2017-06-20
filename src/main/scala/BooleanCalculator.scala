object BooleanCalculator {

    def evaluate(booleanExpr: String): Boolean =
        evaluateAst(buildAst(booleanExpr))

    def astAsString(booleanExpr: String): String =
        formatAst(buildAst(booleanExpr))

    private def buildAst(booleanExpr: String) = {
        val ast: Either[Object, BooleanAst] = for {
            tokens <- BooleanLexer(booleanExpr).right
            ast <- BooleanParser(tokens).right
        } yield ast

        ast.right.get
    }

    private def evaluateAst(ast: BooleanAst): Boolean = ast match {
        case TrueValue => true
        case FalseValue => false
        case NotOp(expr) => !evaluateAst(expr)
        case AndOp(left, right) => evaluateAst(left) && evaluateAst(right)
        case OrOp(left, right) => evaluateAst(left) || evaluateAst(right)
    }

    def formatAst(ast: BooleanAst): String = ast match {
        case TrueValue => "T"
        case FalseValue => "F"
        case NotOp(expr) => s"NOT (${formatAst(expr)})"
        case AndOp(left, right) => s"AND (${formatAst(left)}, ${formatAst(right)})"
        case OrOp(left, right) => s"OR (${formatAst(left)}, ${formatAst(right)})"
    }
}
