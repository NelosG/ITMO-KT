package ru.ifmo.pga.software.design.expression.tokenizer

import ru.ifmo.pga.software.design.expression.token.Operation

class OperationState : SingleCharState<Operation, Operation.Type>() {
    override fun internalCreateToken(type: Operation.Type): Operation {
        return Operation(type)
    }

    override val charToTypeMap = Operation.Type.values().associateBy { it.char }
}