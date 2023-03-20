package ru.ifmo.pga.software.design.reactive

import org.springframework.boot.SpringBootConfiguration
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.data.mongodb.config.AbstractReactiveMongoConfiguration
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories
import org.springframework.transaction.annotation.EnableTransactionManagement

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@SpringBootApplication(scanBasePackages = ["ru.ifmo.pga.software.design"])
@EnableTransactionManagement
open class Application {

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            runApplication<Application>(*args)
        }
    }
}
