package ru.ifmo.pga.software.design.events.statistic

interface EventsStatistic {
    fun incEvent(name: String)
    fun getEventStatisticByName(name: String): Double
    fun getAllEventStatistic(): Map<String, Double>
    fun printStatistic()
}