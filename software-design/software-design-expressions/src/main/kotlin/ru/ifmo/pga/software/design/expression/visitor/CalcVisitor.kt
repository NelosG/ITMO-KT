package ru.ifmo.pga.software.design.expression.visitor

import ru.ifmo.pga.software.design.expression.token.Brace
import ru.ifmo.pga.software.design.expression.token.NumberToken
import ru.ifmo.pga.software.design.expression.token.Operation
import ru.ifmo.pga.software.design.expression.token.Token
import java.util.*

class CalcVisitor : TokenVisitor {
    private val stack: Stack<Token> = Stack()
    override fun visit(token: NumberToken) {
        stack.push(token)
    }

    override fun visit(token: Brace) {
        throw RuntimeException("Parenthesis are not allowed in reversed Polish notation")
    }

    override fun visit(token: Operation) {
        if (stack.size < 2) {
            throw RuntimeException("Incorrect expression")
        }
        val op2 = stack.pop()
        val op1 = stack.pop()
        if (op1 !is NumberToken || op2 !is NumberToken) {
            throw RuntimeException("Incorrect expression")
        }
        val leftValue = op1.value
        val rightValue = op2.value
        stack.push(
            NumberToken(
                when (token.type) {
                    Operation.Type.SUM -> leftValue + rightValue
                    Operation.Type.SUBTRACT -> leftValue - rightValue
                    Operation.Type.MULTIPLY -> leftValue * rightValue
                    Operation.Type.DIVIDE -> leftValue / rightValue
                }
            )
        )
    }

    val result: Long
        get() {
            if (stack.size != 1 || stack[0] !is NumberToken) {
                throw RuntimeException("Incorrect expression")
            }
            return (stack[0] as NumberToken).value
        }
}