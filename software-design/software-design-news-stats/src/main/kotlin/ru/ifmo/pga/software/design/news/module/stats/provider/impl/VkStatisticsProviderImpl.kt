package ru.ifmo.pga.software.design.news.module.stats.provider.impl

import org.apache.commons.lang3.Validate
import ru.ifmo.pga.software.design.news.module.client.search.VkNewsfeedSearchClient
import ru.ifmo.pga.software.design.news.module.stats.parser.impl.VkNewsfeedSearchResponseParser
import ru.ifmo.pga.software.design.news.module.stats.provider.VkStatisticsProvider
import java.time.Duration
import java.time.Instant
import javax.inject.Inject

class VkStatisticsProviderImpl @Inject constructor(
    private val client: VkNewsfeedSearchClient,
    private val parser: VkNewsfeedSearchResponseParser
) : VkStatisticsProvider {

    override fun getPostsByHashtagForLastHours(hashtag: String, startInstant: Instant, hours: Int): LongArray {
        Validate.inclusiveBetween(1, 24, hours.toLong())

        val hashtagCorrected = if (hashtag[0] != '#') {
            "#$hashtag"
        } else hashtag

        val result = LongArray(hours)
        var toInstant = startInstant

        for (i in 0 until hours) {
            val toInstantMinusHour = toInstant.minus(Duration.ofHours(1))
            result[i] = getPostsCountWithHashtagBetween(
                hashtagCorrected,
                toInstantMinusHour.epochSecond,
                toInstant.epochSecond
            )
            toInstant = toInstantMinusHour
        }
        return result
    }

    private fun getPostsCountWithHashtagBetween(hashtag: String, fromUnixSeconds: Long, toUnixSeconds: Long): Long {
        val json = client.fetch(hashtag, fromUnixSeconds, toUnixSeconds)
        val (_, _, totalCount) = parser.parse(json)
        return totalCount
    }
}