package ru.ifmo.pga.software.design.expression.tokenizer

import ru.ifmo.pga.software.design.expression.token.Brace

class BraceState : SingleCharState<Brace, Brace.Type>() {
    override fun internalCreateToken(type: Brace.Type): Brace {
        return Brace(type)
    }

    override val charToTypeMap = Brace.Type.values().associateBy { it.char }
}