package ru.ifmo.pga.software.design.expression.token

import ru.ifmo.pga.software.design.expression.visitor.TokenVisitor

interface Token {
    fun accept(visitor: TokenVisitor)

    interface Type
}