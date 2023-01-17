package ru.ifmo.pga.software.design.expression.visitor

import ru.ifmo.pga.software.design.expression.token.Brace
import ru.ifmo.pga.software.design.expression.token.NumberToken
import ru.ifmo.pga.software.design.expression.token.Operation

interface TokenVisitor {
    fun visit(token: NumberToken)
    fun visit(token: Brace)
    fun visit(token: Operation)
}