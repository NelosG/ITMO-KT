package ru.ifmo.pga.software.design.reactive

import org.springframework.boot.SpringBootConfiguration
import org.springframework.data.mongodb.config.AbstractReactiveMongoConfiguration
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories


@EnableMongoRepositories
@SpringBootConfiguration
open class MongoConfiguration : AbstractReactiveMongoConfiguration() {
    override fun getDatabaseName(): String = "products"
}