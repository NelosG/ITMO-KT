package ru.ifmo.pga.software.design.expression

import ru.ifmo.pga.software.design.expression.token.Token
import ru.ifmo.pga.software.design.expression.tokenizer.Tokenizer
import ru.ifmo.pga.software.design.expression.visitor.CalcVisitor
import ru.ifmo.pga.software.design.expression.visitor.ParserVisitor
import ru.ifmo.pga.software.design.expression.visitor.PrintVisitor
import java.io.InputStream
import java.io.InputStreamReader
import java.io.PrintStream
import java.util.*
import java.util.function.Consumer

class Main(private val `in`: InputStream, private val out: PrintStream, private val err: PrintStream) {
    private fun handleException(e: Exception) {
        out.println("Error: " + e.message)
        e.printStackTrace(err)
    }

    private fun iteration(): Boolean {
        try {
            val scanner = Scanner(InputStreamReader(`in`))
            if (!scanner.hasNextLine()) return false
            val line = scanner.nextLine()
            if ("exit".equals(line, ignoreCase = true)) return false
            val parserVisitor = ParserVisitor()
            Tokenizer.of(line).tokenize().forEach { token -> token.accept(parserVisitor) }
            val rpnTokens: List<Token> = parserVisitor.getOutput()
            val printVisitor: PrintVisitor = PrintVisitor.of(out)
            rpnTokens.forEach(Consumer { token: Token -> token.accept(printVisitor) })
            out.println()
            val calcVisitor = CalcVisitor()
            rpnTokens.forEach(Consumer { token: Token -> token.accept(calcVisitor) })
            out.println("Value: " + calcVisitor.result)
        } catch (e: Exception) {
            handleException(e)
        }
        return true
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            val main = Main(System.`in`, System.out, System.err)
            while (main.iteration()) {
            }
        }
    }
}