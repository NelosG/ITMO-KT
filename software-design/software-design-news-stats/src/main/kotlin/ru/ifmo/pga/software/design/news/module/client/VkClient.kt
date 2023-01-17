package ru.ifmo.pga.software.design.news.module.client

import ru.ifmo.pga.software.design.news.module.client.exception.VkClientException
import java.io.IOException
import java.io.UncheckedIOException
import java.net.HttpURLConnection
import java.net.URI
import java.net.URISyntaxException
import java.net.URLEncoder
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.nio.charset.StandardCharsets

abstract class VkClient(
    private val secure: Boolean,
    private val host: String,
    private val port: Int,
    private val method: String,
    private val accessToken: String
) {
    fun fetch(params: Map<String, String>): String {
        val client = HttpClient.newBuilder()
            .version(HttpClient.Version.HTTP_1_1)
            .build()
        val request = HttpRequest.newBuilder(
            getUri(params)
        ).GET().build()

        val response: HttpResponse<String> = try {
            client.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
        } catch (e: InterruptedException) {
            Thread.currentThread().interrupt()
            throw RuntimeException(e)
        } catch (e: IOException) {
            throw UncheckedIOException(e)
        }
        if (response.statusCode() != HttpURLConnection.HTTP_OK) {
            throw VkClientException("Vk returned not ok status code: " + response.statusCode())
        }
        return response.body()
    }

    private fun getUri(params: Map<String, String>): URI {
        val uriBuilder = StringBuilder()
            .append(if (secure) "https" else "http").append("://")
            .append(encode(host)).append(":").append(port)
            .append("/method/").append(method)
            .append("?")
            .append("access_token=").append(accessToken)
            .append("&v=").append(API_VERSION)
        params.forEach { (key: String, value: String) ->
            uriBuilder.append("&").append(encode(key)).append("=").append(encode(value))
        }
        return try {
            URI(uriBuilder.toString())
        } catch (e: URISyntaxException) {
            throw RuntimeException(e)
        }
    }

    private fun encode(toEncode: String): String {
        return URLEncoder.encode(toEncode, StandardCharsets.UTF_8)
    }

    companion object {
        const val API_VERSION = "5.131"
    }
}