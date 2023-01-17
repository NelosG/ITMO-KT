package ru.ifmo.pga.software.design.events.clock.impl

import ru.ifmo.pga.software.design.events.clock.Clock
import java.time.Instant

class ClockImpl(private var now: Instant) : Clock {

    override fun now(): Instant {
        return now
    }
}