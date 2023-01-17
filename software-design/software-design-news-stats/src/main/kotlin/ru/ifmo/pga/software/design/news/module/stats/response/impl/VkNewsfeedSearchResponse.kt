package ru.ifmo.pga.software.design.news.module.stats.response.impl

import ru.ifmo.pga.software.design.news.module.stats.response.VkResponse
import java.util.*

data class VkNewsfeedSearchResponse(
    val count: Int,
    val nextFrom: Optional<String>,
    val totalCount: Long
) : VkResponse
