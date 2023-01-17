package ru.ifmo.pga.software.design.news.module.stats.parser

import com.google.common.io.Resources
import com.google.gson.JsonSyntaxException
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import ru.ifmo.pga.software.design.news.module.stats.parser.exception.VkResponseParserException
import ru.ifmo.pga.software.design.news.module.stats.parser.impl.VkNewsfeedSearchResponseParser
import java.io.IOException
import java.io.UncheckedIOException
import java.nio.charset.StandardCharsets

class VkNewsfeedSearchResponseParserTest {
    private lateinit var parser: VkNewsfeedSearchResponseParser

    @BeforeEach
    fun initParser() {
        parser = VkNewsfeedSearchResponseParser()
    }

    @Test
    fun ok() {
        val result = parser.parse(readTest("Ok"))
        assertEquals(133, result.count)
        assertEquals(133, result.totalCount)
        assertTrue(result.nextFrom.isPresent)
        assertEquals("30/-175249128_10956", result.nextFrom.get())
    }

    @Test
    fun okNoNextFrom() {
        val result = parser.parse(readTest("OkNoNextFrom"))
        assertEquals(5, result.count)
        assertEquals(15, result.totalCount)
        assertFalse(result.nextFrom.isPresent)
    }

    @Test
    fun apiError() {
        assertThrows<VkResponseParserException> {
            parser.parse(readTest("ApiError"))
        }
    }

    @Test
    fun malformedJson() {
        assertThrows<JsonSyntaxException> {
            parser.parse(readTest("MalformedJson"))
        }
    }

    @Test
    fun noFields() {
        assertThrows<IllegalArgumentException> {
            parser.parse(readTest("NoFields"))
        }
    }

    private fun readTest(name: String): String {
        val resource = javaClass.getResource("parserTest$name.json")
        assertNotNull(resource)
        return try {
            Resources.toString(resource!!, StandardCharsets.UTF_8)
        } catch (e: IOException) {
            throw UncheckedIOException(e)
        }
    }
}