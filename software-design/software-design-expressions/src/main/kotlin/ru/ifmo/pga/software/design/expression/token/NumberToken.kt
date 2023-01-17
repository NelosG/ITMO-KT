package ru.ifmo.pga.software.design.expression.token

import ru.ifmo.pga.software.design.expression.visitor.TokenVisitor

data class NumberToken(val value: Long) : Token {

    override fun accept(visitor: TokenVisitor) {
        visitor.visit(this)
    }
}