
object BooleanParser {
    def apply(token: Token): Boolean = {
        token match {
            case BOOLEAN_VAL(booleanVal) =>
                if (booleanVal.equals("T")) true
                else false
        }
    }
}
