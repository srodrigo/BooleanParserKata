trait BooleanCompilationError
case class BooleanLexerError(msg: String) extends BooleanCompilationError
case class BooleanParserError(msg: String) extends BooleanCompilationError

