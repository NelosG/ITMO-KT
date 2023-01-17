package ru.ifmo.pga.software.design.news

import com.google.inject.Guice
import ru.ifmo.pga.software.design.news.module.VkStatisticsModule
import ru.ifmo.pga.software.design.news.module.stats.provider.VkStatisticsProvider
import java.text.DecimalFormat
import java.text.NumberFormat
import java.time.Instant


fun main() {
    val injector = Guice.createInjector(VkStatisticsModule())
    val provider = injector.getInstance(VkStatisticsProvider::class.java)


    val n = 24
    val result = provider.getPostsByHashtagForLastHours("Украина", Instant.now(), n)

    val f: NumberFormat = DecimalFormat("00")
    for (i in 0 until n) {
        println("${f.format(i + 1)} час(ов) назад: ${result[i]} новостей")
    }
}
