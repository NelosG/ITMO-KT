package ru.ifmo.pga.software.design.news.module.stats.provider

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.Mock
import org.mockito.Mockito.*
import org.mockito.junit.jupiter.MockitoExtension
import ru.ifmo.pga.software.design.news.module.client.search.VkNewsfeedSearchClient
import ru.ifmo.pga.software.design.news.module.stats.parser.impl.VkNewsfeedSearchResponseParser
import ru.ifmo.pga.software.design.news.module.stats.provider.impl.VkStatisticsProviderImpl
import java.time.Instant
import java.util.*

@ExtendWith(MockitoExtension::class)
class VkStatisticsProviderImplTest {
    @Mock
    private lateinit var vkNewsfeedSearchClient: VkNewsfeedSearchClient

    @Test
    fun simple() {
        `when`(
            vkNewsfeedSearchClient.fetch(
                "#test",
                CUR_TIME_SEC - HOUR_IN_SECONDS,
                CUR_TIME_SEC
            )
        ).thenReturn(generateAnswer(5))

        val provider: VkStatisticsProvider =
            VkStatisticsProviderImpl(vkNewsfeedSearchClient, VkNewsfeedSearchResponseParser())

        val statistics = provider.getPostsByHashtagForLastHours("test", CUR_TIME, 1)

        verify(vkNewsfeedSearchClient, times(1)).fetch(
            "#test",
            CUR_TIME_SEC - HOUR_IN_SECONDS,
            CUR_TIME_SEC
        )

        assertEquals(5, statistics[0])
    }

    @Test
    fun manyHours() {
        `when`(
            vkNewsfeedSearchClient.fetch(
                "#test",
                CUR_TIME_SEC - HOUR_IN_SECONDS,
                CUR_TIME_SEC
            )
        ).thenReturn(generateAnswer(5))

        `when`(
            vkNewsfeedSearchClient.fetch(
                "#test",
                CUR_TIME_SEC - 2 * HOUR_IN_SECONDS,
                CUR_TIME_SEC - HOUR_IN_SECONDS
            )
        ).thenReturn(generateAnswer(8))

        `when`(
            vkNewsfeedSearchClient.fetch(
                "#test",
                CUR_TIME_SEC - 3 * HOUR_IN_SECONDS,
                CUR_TIME_SEC - 2 * HOUR_IN_SECONDS
            )
        ).thenReturn(generateAnswer(15))

        val provider: VkStatisticsProvider =
            VkStatisticsProviderImpl(vkNewsfeedSearchClient, VkNewsfeedSearchResponseParser())
        val statistics = provider.getPostsByHashtagForLastHours("test", CUR_TIME, 3)

        verify(vkNewsfeedSearchClient, times(3)).fetch(anyString(), anyLong(), anyLong())

        assertEquals(5, statistics[0])
        assertEquals(8, statistics[1])
        assertEquals(15, statistics[2])
    }

    private fun generateAnswer(count: Int): String {
        val items = StringBuilder()
        items.append("{},".repeat(count))
        items.deleteCharAt(items.length - 1)
        return """
                {  "response": {
                    "count": $count,
                    "items": [
                      $items
                    ],
                    "total_count": $count
                  }
                }"""
    }

    companion object {
        private val CUR_TIME = Instant.now()
        private val CUR_TIME_SEC = CUR_TIME.epochSecond
        private const val HOUR_IN_SECONDS = 60 * 60
    }
}