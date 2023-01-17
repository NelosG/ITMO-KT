package ru.ifmo.pga.software.design.news.module

import com.google.inject.AbstractModule
import com.google.inject.name.Names
import ru.ifmo.pga.software.design.news.module.stats.provider.VkStatisticsProvider
import ru.ifmo.pga.software.design.news.module.stats.provider.impl.VkStatisticsProviderImpl

class VkStatisticsModule : AbstractModule() {


    override fun configure() {
        bind(VkStatisticsProvider::class.java).to(VkStatisticsProviderImpl::class.java)

        bind(Boolean::class.java).annotatedWith(Names.named("vkSecure")).toInstance(true)
        bind(String::class.java).annotatedWith(Names.named("vkHost")).toInstance("api.vk.com")
        bind(Int::class.java).annotatedWith(Names.named("vkPort")).toInstance(443)
        bind(String::class.java).annotatedWith(Names.named("vkAccessToken"))
            .toInstance(System.getenv("VK_ACCESS_TOKEN"))
    }
}