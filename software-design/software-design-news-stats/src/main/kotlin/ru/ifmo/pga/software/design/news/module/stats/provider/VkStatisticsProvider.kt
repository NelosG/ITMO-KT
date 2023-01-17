package ru.ifmo.pga.software.design.news.module.stats.provider

import java.time.Instant

interface VkStatisticsProvider {
    fun getPostsByHashtagForLastHours(hashtag: String, startInstant: Instant, hours: Int): LongArray
}