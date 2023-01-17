package ru.ifmo.pga.software.design.expression.visitor

import ru.ifmo.pga.software.design.expression.token.Brace
import ru.ifmo.pga.software.design.expression.token.NumberToken
import ru.ifmo.pga.software.design.expression.token.Operation
import java.io.OutputStream
import java.io.PrintStream

class PrintVisitor(private val printStream: PrintStream) : TokenVisitor {
    private var start = true
    private fun printWhiteSpace() {
        if (start) start = false else printStream.print(" ")
    }

    override fun visit(token: NumberToken) {
        printWhiteSpace()
        val value = token.value
        printStream.print(
            value
        )
    }

    override fun visit(token: Brace) {
        printWhiteSpace()
        printStream.print(token.type.char)
    }

    override fun visit(token: Operation) {
        printWhiteSpace()
        printStream.print(token.type.char)
    }

    companion object {
        fun of(outputStream: OutputStream): PrintVisitor {
            return PrintVisitor(PrintStream(outputStream))
        }
    }
}