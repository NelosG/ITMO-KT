package ru.ifmo.pga.software.design.expression.tokenizer

import ru.ifmo.pga.software.design.expression.token.Token


abstract class SingleCharState<T : Token, TT : Token.Type> : State<T> {
    private var token: T? = null
    override fun process(ch: Char) {
        if (token != null) {
            throw IllegalStateException(EXCEPTION_ILLEGAL_STATE_MESSAGE)
        }
        token = internalCreateToken(charToTypeMap[ch] ?: error(EXCEPTION_ILLEGAL_ARGUMENT_MESSAGE))
    }

    override fun getToken(): T {
        return token ?: throw IllegalStateException(EXCEPTION_ILLEGAL_STATE_MESSAGE)
    }

    protected abstract val charToTypeMap: Map<Char, TT>
    protected abstract fun internalCreateToken(type: TT): T
    override fun isCharAcceptable(ch: Char): Boolean {
        return charToTypeMap.containsKey(ch)
    }

    override fun reset() {
        token = null
    }

    override fun needFinalize(ch: Char?): Boolean {
        return token != null
    }

    companion object {
        private const val EXCEPTION_ILLEGAL_STATE_MESSAGE = "process() must be called exactly once"
        private const val EXCEPTION_ILLEGAL_ARGUMENT_MESSAGE = "Token missing"
    }
}