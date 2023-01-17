package ru.ifmo.pga.software.design.news.module.client

import com.xebialabs.restito.builder.stub.StubHttp
import com.xebialabs.restito.semantics.Action
import com.xebialabs.restito.semantics.Call
import com.xebialabs.restito.semantics.Condition
import com.xebialabs.restito.server.StubServer
import org.glassfish.grizzly.http.Method
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import ru.ifmo.pga.software.design.news.module.client.exception.VkClientException
import java.util.*

class VkClientTest {
    private lateinit var stubServer: StubServer

    @BeforeEach
    fun createServer() {
        stubServer = StubServer(PORT).run()
    }

    @AfterEach
    fun shutdownServer() {
        stubServer.stop()
    }

    @Test
    fun simplePingPong() {
        StubHttp.whenHttp(stubServer)
            .match(Condition.method(Method.GET), Condition.startsWithUri("/method/"))
            .then(Action.stringContent("OK"))

        val client: VkClient = TestVkClient("user.info", ACCESS_TOKEN)
        val result = client.fetch(mapOf())
        assertEquals("OK", result)
    }

    @Test
    fun params() {
        val query = "query"
        val startTime = "1666547983"
        val endTime = "1666548000"

        StubHttp.whenHttp(stubServer)
            .match(
                Condition.method(Method.GET),
                Condition.startsWithUri("/method/"),
                Condition.parameter("q", query),
                Condition.parameter("start_time", startTime),
                Condition.parameter("end_time", endTime),
                Condition.parameter("access_token", ACCESS_TOKEN),
                Condition.parameter("v", VkClient.API_VERSION),
                Condition.custom { call: Call -> call.parameters.size == 5 }
            ).then(Action.stringContent("OK"))
        val client: VkClient = TestVkClient("newsfeed.search", "abacaba")
        val result = client.fetch(
            mapOf(
                "q" to query,
                "start_time" to startTime,
                "end_time" to endTime
            )
        )
        assertEquals("OK", result)
    }

    @Test
    fun httpNot200() {
        StubHttp.whenHttp(stubServer).match(Condition.alwaysFalse()).then()
        val client: VkClient = TestVkClient("user.info", ACCESS_TOKEN)

        assertThrows<VkClientException> {
            client.fetch(mapOf())
        }
    }

    @Test
    fun illegalUri() {
        val client: VkClient = TestVkClient("\\\\", "\\\\")
        assertThrows<RuntimeException> {
            client.fetch(mapOf())
        }
    }

    private class TestVkClient(method: String, accessToken: String) :
        VkClient(false, "localhost", PORT, method, accessToken)

    companion object {
        private val PORT = Random().nextInt(20000, 65535)
        private const val ACCESS_TOKEN = "abacaba"
    }
}