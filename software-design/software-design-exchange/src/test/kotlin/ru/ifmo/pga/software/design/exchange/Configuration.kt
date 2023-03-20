package ru.ifmo.pga.software.design.exchange

import org.hibernate.SessionFactory
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.SpringApplication
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.ComponentScan
import org.springframework.context.annotation.FilterType
import org.springframework.core.env.Environment
import org.springframework.jdbc.datasource.DriverManagerDataSource
import org.springframework.orm.hibernate5.LocalSessionFactoryBean
import org.springframework.test.context.TestPropertySource
import org.springframework.transaction.annotation.EnableTransactionManagement
import ru.ifmo.pga.software.design.core.Application
import java.util.*
import javax.sql.DataSource


@SpringBootApplication(
    scanBasePackages = [
        "ru.ifmo.pga.software.design.core",
        "ru.ifmo.pga.software.design.exchange.service",
        "ru.ifmo.pga.software.design.exchange.entity",
        "ru.ifmo.pga.software.design.exchange.dao"
    ]
)
@ComponentScan(
    excludeFilters = [
        ComponentScan.Filter(
            type = FilterType.ASSIGNABLE_TYPE, classes = [Application::class]
        )
    ]
)
@EnableTransactionManagement
@TestPropertySource(locations = ["classpath:application-integrationtest.properties"])
open class Configuration {

    @Autowired
    lateinit var env: Environment

    @Bean("dataSource")
    open fun getDataSource(): DataSource? {
        val dataSource = DriverManagerDataSource()
        env.getProperty("spring.datasource.driver-class-name")?.let { dataSource.setDriverClassName(it) }
        dataSource.url = "jdbc:postgresql://localhost:${DB_PORT}/${DB_NAME}"
        dataSource.username = USERNAME
        dataSource.password = PASSWORD
        return dataSource
    }

    @Autowired
    @Bean("SoftwareDesignUnit")
    open fun getSessionFactory(dataSource: DataSource?): SessionFactory? {
        val properties = Properties()
        properties["hibernate.dialect"] = env.getProperty("spring.jpa.properties.hibernate.dialect")
        properties["hibernate.show_sql"] = env.getProperty("spring.jpa.show-sql")
        properties["current_session_context_class"] =
            env.getProperty("spring.jpa.properties.hibernate.current_session_context_class")
        val factoryBean = LocalSessionFactoryBean()
        factoryBean.setPackagesToScan("ru.ifmo.pga.software.design")
        if (dataSource != null) {
            factoryBean.setDataSource(dataSource)
        }
        factoryBean.hibernateProperties = properties
        factoryBean.afterPropertiesSet()
        return factoryBean.getObject()
    }

    companion object {
        const val DB_NAME = "exchange"
        const val USERNAME = "postgres"
        const val PASSWORD = "postgres"
        const val DB_PORT = 5454

        @JvmStatic
        fun main(args: Array<String>) {
            SpringApplication.run(Application::class.java, *args)
        }
    }
}