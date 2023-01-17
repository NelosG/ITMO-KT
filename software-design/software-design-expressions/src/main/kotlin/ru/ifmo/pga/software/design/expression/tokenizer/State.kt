package ru.ifmo.pga.software.design.expression.tokenizer

import ru.ifmo.pga.software.design.expression.token.Token

internal interface State<T : Token?> {
    fun process(ch: Char)
    fun getToken(): T
    fun isCharAcceptable(ch: Char): Boolean
    fun reset()
    fun needFinalize(ch: Char? = null): Boolean
}