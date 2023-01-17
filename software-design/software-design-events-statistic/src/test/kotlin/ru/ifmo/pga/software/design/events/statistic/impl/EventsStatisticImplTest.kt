package ru.ifmo.pga.software.design.events.statistic.impl

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.mockito.Mockito.mock
import org.mockito.Mockito.`when`
import ru.ifmo.pga.software.design.events.clock.Clock
import ru.ifmo.pga.software.design.events.statistic.EventsStatistic
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.time.Duration
import java.time.Instant

class EventsStatisticImplTest {
    private val instant = Instant.now()
    private val clock: Clock = mock(Clock::class.java)
    private lateinit var eventsStatistic: EventsStatistic

    @BeforeEach
    fun setUp() {
        `when`(clock.now()).thenReturn(instant)
        eventsStatistic = EventsStatisticImpl(clock)
    }

    @Test
    fun incEvent() {
        eventsStatistic.incEvent("event")

        val eventFrequency = eventsStatistic.getEventStatisticByName("event")

        assertEquals(1.0 / 60, eventFrequency)
    }

    @Test
    fun incEventWithDelay() {
        eventsStatistic.incEvent("event")

        `when`(clock.now()).thenReturn(
            instant.plus(Duration.ofMinutes(61))
        )
        eventsStatistic.incEvent("event")

        val eventFrequency = eventsStatistic.getEventStatisticByName("event")

        assertEquals(1.0 / 60, eventFrequency)
    }

        @Test
    fun nonExistentEvent() {
        val eventFrequency = eventsStatistic.getEventStatisticByName("event")

        assertEquals(0.0, eventFrequency)
    }

    @Test
    fun oldMustBeRemoved() {
        eventsStatistic.incEvent("event")

        `when`(clock.now()).thenReturn(
            instant.plus(Duration.ofHours(1))
                .plus(Duration.ofNanos(1))
        )

        val eventFrequency = eventsStatistic.getEventStatisticByName("event")

        assertEquals(0.0, eventFrequency)
    }

    @Test
    fun severalEvents() {
        repeat(15) { eventsStatistic.incEvent("event1") }
        repeat(100) { eventsStatistic.incEvent("event2") }
        repeat(60) { eventsStatistic.incEvent("event3") }

        val eventsFrequency = eventsStatistic.getAllEventStatistic()

        assertEquals(
            mapOf("event1" to 15.0 / 60, "event2" to 100.0 / 60, "event3" to 60.0 / 60),
            eventsFrequency
        )
    }

        @Test
    fun severalEventsWithDelay() {
        repeat(30) { eventsStatistic.incEvent("event") }
        `when`(clock.now()).thenReturn(
            instant.plus(Duration.ofHours(1))
                .plus(Duration.ofNanos(1))
        )
        repeat(30) { eventsStatistic.incEvent("event") }

        val eventFrequency = eventsStatistic.getEventStatisticByName("event")

        assertEquals(30.0 / 60, eventFrequency)
    }

    @Test
    fun severalEventsInDifferentTime() {
        repeat(15) { eventsStatistic.incEvent("event1") }

        `when`(clock.now()).thenReturn(
            instant.plus(Duration.ofMinutes(30))
        )
        repeat(100) { eventsStatistic.incEvent("event2") }

        `when`(clock.now()).thenReturn(
            instant.plus(Duration.ofMinutes(91))
        )
        repeat(60) { eventsStatistic.incEvent("event3") }

        val eventsFrequency = eventsStatistic.getAllEventStatistic()

        assertEquals(
            mapOf("event3" to 60.0 / 60),
            eventsFrequency
        )
    }

    @Test
    fun printStatistic() {
        val first = 11
        val second = 22
        val third = 33
        val fourth = 44
        repeat(first) { eventsStatistic.incEvent("event1") }
        repeat(second) { eventsStatistic.incEvent("event2") }
        repeat(third) { eventsStatistic.incEvent("event3") }
        repeat(fourth) { eventsStatistic.incEvent("event4") }
        val outputStream = ByteArrayOutputStream()

        PrintStream(outputStream).use {
            System.setOut(it)
            eventsStatistic.printStatistic()
        }

        val separator = System.lineSeparator()
        assertEquals(
            "event1 : ${first / 60.0}$separator" +
                    "event2 : ${second / 60.0}$separator" +
                    "event3 : ${third / 60.0}$separator" +
                    "event4 : ${fourth / 60.0}$separator",
            outputStream.toString()
        )
    }
}