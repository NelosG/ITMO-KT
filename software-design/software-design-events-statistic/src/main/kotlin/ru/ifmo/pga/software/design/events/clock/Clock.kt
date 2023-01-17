package ru.ifmo.pga.software.design.events.clock

import java.time.Duration
import java.time.Instant

interface Clock {
    companion object {
        val HOUR: Duration = Duration.ofHours(1)
        const val MINUTES_IN_HOUR = 60
    }

    fun now(): Instant
}