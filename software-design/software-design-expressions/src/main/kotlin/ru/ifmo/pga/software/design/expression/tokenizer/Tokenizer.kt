package ru.ifmo.pga.software.design.expression.tokenizer

import ru.ifmo.pga.software.design.expression.token.Token
import java.io.*

class Tokenizer(private val reader: Reader) {
    private val states = listOf(
        BraceState(), OperationState(), NumberState()
    )
    private val tokens: MutableList<Token> = ArrayList()
    private var position = 0
    private var state: State<out Token>? = null
    fun tokenize(): List<Token> {
        while (true) {
            val ch = nextNonWhitespace()
            if (ch == null) {
                tryFinalizeState()
                return tokens
            }
            updateState(ch)
            state!!.process(ch)
        }
    }

    private fun nextChar(): Char? {
        return try {
            val code = reader.read()
            if (code == -1) return null
            position++
            val chars = Character.toChars(code)
            if (chars.size > 1) throwException()
            chars[0]
        } catch (e: IOException) {
            throw UncheckedIOException(e)
        }
    }

    private fun nextNonWhitespace(): Char? {
        var ch: Char?
        while (true) {
            ch = nextChar()
            if (ch == null) return null
            if (!Character.isWhitespace(ch)) return ch else tryFinalizeState(ch)
        }
    }

    private fun tryFinalizeState(ch: Char? = null) {
        if (state != null && state!!.needFinalize(ch)) {
            tokens.add(state!!.getToken())
            state!!.reset()
        }
    }

    private fun updateState(ch: Char) {
        for (state in states) {
            if (state.isCharAcceptable(ch)) {
                tryFinalizeState(ch)
                this.state = state
                return
            }
        }
        throwException()
    }

    private fun throwException() {
        throw RuntimeException("Can't parse: unexpected char at position $position")
    }

    companion object {
        fun of(string: String): Tokenizer {
            return Tokenizer(StringReader(string))
        }

        fun of(inputStream: InputStream): Tokenizer {
            return Tokenizer(InputStreamReader(inputStream))
        }
    }
}