package ru.ifmo.pga.software.design.news.module.client.search

import com.google.inject.AbstractModule
import com.google.inject.Guice
import com.google.inject.Injector
import com.google.inject.name.Names
import com.xebialabs.restito.builder.stub.StubHttp
import com.xebialabs.restito.semantics.Action
import com.xebialabs.restito.semantics.Call
import com.xebialabs.restito.semantics.Condition
import com.xebialabs.restito.server.StubServer
import org.glassfish.grizzly.http.Method
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import ru.ifmo.pga.software.design.news.module.client.VkClient
import ru.ifmo.pga.software.design.news.module.stats.provider.VkStatisticsProvider
import ru.ifmo.pga.software.design.news.module.stats.provider.impl.VkStatisticsProviderImpl
import java.util.*

class VkNewsfeedSearchClientTest {
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
    fun simple() {
        val query = "query"
        val startTime = 1666547983L
        val endTime = 1666548000L

        StubHttp.whenHttp(stubServer)
            .match(
                Condition.method(Method.GET),
                Condition.startsWithUri("/method/"),
                Condition.parameter("q", query),
                Condition.parameter("start_time", startTime.toString()),
                Condition.parameter("end_time", endTime.toString()),
                Condition.parameter("access_token", ACCESS_TOKEN),
                Condition.parameter("v", VkClient.API_VERSION),
                Condition.custom { call: Call -> call.parameters.size == 5 }
            ).then(Action.stringContent("OK"))

        val client = injector.getInstance(
            VkNewsfeedSearchClient::class.java
        )
        val response = client.fetch(query, startTime, endTime)
        assertEquals("OK", response)
    }

    class VkNewsfeedSearchClientTestModule : AbstractModule() {
        override fun configure() {
            bind(VkStatisticsProvider::class.java).to(VkStatisticsProviderImpl::class.java)

            bind(Boolean::class.java).annotatedWith(Names.named("vkSecure")).toInstance(false)
            bind(String::class.java).annotatedWith(Names.named("vkHost")).toInstance("localhost")
            bind(Int::class.java).annotatedWith(Names.named("vkPort")).toInstance(PORT)
            bind(String::class.java).annotatedWith(Names.named("vkAccessToken")).toInstance(ACCESS_TOKEN)
        }
    }

    companion object {
        private val PORT = Random().nextInt(20000, 65535)
        private const val ACCESS_TOKEN = "abacaba"
        private lateinit var injector: Injector

        @JvmStatic
        @BeforeAll
        fun prepareGuice() {
            injector = Guice.createInjector(VkNewsfeedSearchClientTestModule())
        }
    }
}