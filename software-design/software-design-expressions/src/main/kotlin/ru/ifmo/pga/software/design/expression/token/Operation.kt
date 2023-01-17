package ru.ifmo.pga.software.design.expression.token

import ru.ifmo.pga.software.design.expression.visitor.TokenVisitor

data class Operation(val type: Type) : Token {
    override fun accept(visitor: TokenVisitor) {
        visitor.visit(this)
    }

    enum class Type constructor(val priority: Int, val char: Char) : Token.Type {
        SUM(1, '+'),
        SUBTRACT(1, '-'),
        MULTIPLY(0, '*'),
        DIVIDE(0, '/');
    }
}
