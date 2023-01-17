package ru.ifmo.pga.software.design.news.module.client.search

import ru.ifmo.pga.software.design.news.module.client.VkClient
import javax.inject.Inject
import javax.inject.Named

class VkNewsfeedSearchClient @Inject constructor(
    @Named("vkSecure") secure: Boolean,
    @Named("vkHost") host: String,
    @Named("vkPort") port: Int,
    @Named("vkAccessToken") accessToken: String
) : VkClient(secure, host, port, METHOD, accessToken) {

    fun fetch(query: String, startTimeUnixSeconds: Long, endTimeUnixSeconds: Long): String {
        val params = mutableMapOf(
            "q" to query,
            "start_time" to startTimeUnixSeconds.toString(),
            "end_time" to endTimeUnixSeconds.toString()
        )
        return super.fetch(params)
    }

    companion object {
        private const val METHOD = "newsfeed.search"
    }
}