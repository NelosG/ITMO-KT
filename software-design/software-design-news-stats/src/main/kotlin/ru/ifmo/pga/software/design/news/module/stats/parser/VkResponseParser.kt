package ru.ifmo.pga.software.design.news.module.stats.parser

import ru.ifmo.pga.software.design.news.module.stats.response.VkResponse

interface VkResponseParser<T : VkResponse> {
    fun parse(apiResponse: String): T
}