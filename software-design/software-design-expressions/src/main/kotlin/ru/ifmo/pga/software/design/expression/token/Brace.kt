package ru.ifmo.pga.software.design.expression.token

import ru.ifmo.pga.software.design.expression.visitor.TokenVisitor

data class Brace(val type: Type) : Token {
    override fun accept(visitor: TokenVisitor) {
        visitor.visit(this)
    }

    enum class Type(val char: Char) : Token.Type {
        LEFT('('),
        RIGHT(')')
    }
}