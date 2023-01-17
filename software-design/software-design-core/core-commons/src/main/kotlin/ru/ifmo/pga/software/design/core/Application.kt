package ru.ifmo.pga.software.design.core

import org.hibernate.SessionFactory
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.SpringApplication
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.core.env.Environment
import org.springframework.jdbc.datasource.DriverManagerDataSource
import org.springframework.orm.hibernate5.LocalSessionFactoryBean
import org.springframework.transaction.annotation.EnableTransactionManagement
import java.util.*
import javax.sql.DataSource

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@SpringBootApplication(scanBasePackages = ["ru.ifmo.pga.software.design"])
@Configuration
@EnableTransactionManagement
open class Application {

    @Autowired
    lateinit var env: Environment

    val logger = LoggerFactory.getLogger(javaClass)

    @Bean("dataSource")
    open fun getDataSource(): DataSource? {
        val dataSource = DriverManagerDataSource()
        env.getProperty("spring.datasource.driver-class-name")?.let { dataSource.setDriverClassName(it) }
        dataSource.url = env.getProperty("spring.datasource.url")
        dataSource.username = env.getProperty("spring.datasource.username")
        dataSource.password = env.getProperty("spring.datasource.password")
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
        @JvmStatic
        fun main(args: Array<String>) {
            SpringApplication.run(Application::class.java, *args)
        }
    }
}
