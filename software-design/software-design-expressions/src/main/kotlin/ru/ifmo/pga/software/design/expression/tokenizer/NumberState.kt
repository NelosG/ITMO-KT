package ru.ifmo.pga.software.design.expression.tokenizer

import ru.ifmo.pga.software.design.expression.token.NumberToken

class NumberState : State<NumberToken> {
    private var number: Long? = null

    override fun process(ch: Char) {
        if (!isCharAcceptable(ch)) {
            error("Illegal argument")
        }
        number = number?.times(10) ?: 0
        number = number!! + (ch.code - '0'.code).toLong()
    }

    override fun getToken(): NumberToken {
        return NumberToken(number ?: error("Number missing"))
    }

    override fun isCharAcceptable(ch: Char): Boolean {
        return ch in '0'..'9'
    }

    override fun reset() {
        number = null
    }

    override fun needFinalize(ch: Char?): Boolean {
        return number != null && (ch == null || !isCharAcceptable(ch))
    }
}