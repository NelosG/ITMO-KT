package ru.ifmo.pga.software.design.expression.visitor

import ru.ifmo.pga.software.design.expression.token.Brace
import ru.ifmo.pga.software.design.expression.token.NumberToken
import ru.ifmo.pga.software.design.expression.token.Operation
import ru.ifmo.pga.software.design.expression.token.Token
import java.util.*

class ParserVisitor : TokenVisitor {
    private val output: MutableList<Token> = ArrayList()
    private val stack: Stack<Token> = Stack()
    override fun visit(token: NumberToken) {
        output.add(token)
    }

    override fun visit(token: Brace) {
        when (token.type) {
            Brace.Type.LEFT -> {
                stack.push(token)
            }

            Brace.Type.RIGHT -> {
                while (true) {
                    if (stack.isEmpty()) {
                        throw RuntimeException("Missing parenthesis")
                    }
                    val topToken = stack.pop()
                    if (topToken !is Brace) output.add(topToken) else break
                }
            }
        }
    }

    override fun visit(token: Operation) {
        while (!stack.isEmpty()) {
            val topToken = stack.peek()
            if (topToken is Operation) {
                if (topToken.type.priority <= token.type.priority) {
                    output.add(stack.pop())
                } else break
            } else break
        }
        stack.push(token)
    }

    fun getOutput(): List<Token> {
        while (stack.isNotEmpty()) {
            val topToken = stack.pop()
            if (topToken is Brace) {
                throw RuntimeException("Missing parenthesis")
            }
            output.add(topToken)
        }
        return output
    }
}