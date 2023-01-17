package ru.ifmo.pga.software.design.events.statistic.impl

import ru.ifmo.pga.software.design.events.clock.Clock
import ru.ifmo.pga.software.design.events.statistic.EventsStatistic
import java.time.Instant

class EventsStatisticImpl(private val clock: Clock) : EventsStatistic {
    private val statistics = mutableMapOf<String, ArrayDeque<Instant>>()

    override fun incEvent(name: String) {
        statistics.getOrPut(name) { ArrayDeque() }.add(clock.now())
    }

    override fun getEventStatisticByName(name: String): Double {
        removeOld(name)
        return (statistics[name]?.size ?: 0) / Clock.MINUTES_IN_HOUR.toDouble()
    }

    override fun getAllEventStatistic(): Map<String, Double> {
        statistics.toList().forEach { (name, _) -> removeOld(name) }
        return statistics.mapValues { (_, events) -> events.size.toDouble() / Clock.MINUTES_IN_HOUR }
    }

    override fun printStatistic() {
        getAllEventStatistic().forEach { println("${it.key} : ${it.value}") }
    }

    private fun removeOld(name: String) {
        val events = statistics[name] ?: return
        while (events.firstOrNull()?.isBefore(clock.now().minus(Clock.HOUR)) == true) {
            events.removeFirst()
        }

        if (events.isEmpty()) {
            statistics.remove(name)
        }
    }
}