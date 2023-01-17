package ru.akirakozov.sd.refactoring

import org.eclipse.jetty.server.Server
import org.hibernate.SessionFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.CommandLineRunner
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration
import org.springframework.boot.autoconfigure.jdbc.DataSourceTransactionManagerAutoConfiguration
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration
import org.springframework.context.ApplicationContext
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.core.env.Environment
import org.springframework.jdbc.datasource.DriverManagerDataSource
import org.springframework.orm.hibernate5.HibernateTransactionManager
import org.springframework.orm.hibernate5.LocalSessionFactoryBean
import org.springframework.transaction.annotation.EnableTransactionManagement
import ru.akirakozov.sd.refactoring.dao.ProductDao
import ru.akirakozov.sd.refactoring.server.ApplicationServer
import java.util.*
import javax.sql.DataSource


@SpringBootApplication(
    exclude = [ //
        DataSourceAutoConfiguration::class,  //
        DataSourceTransactionManagerAutoConfiguration::class,  //
        HibernateJpaAutoConfiguration::class]
)
@Configuration
@EnableTransactionManagement
open class Application : CommandLineRunner {

    @Autowired
    lateinit var env: Environment

    @Autowired
    lateinit var context: ApplicationContext

    @Bean("dataSource")
    open fun getDataSource(): DataSource? {
        val dataSource = DriverManagerDataSource()

        // See: application.properties
        env.getProperty("spring.datasource.driver-class-name")?.let { dataSource.setDriverClassName(it) }
        dataSource.url = env.getProperty("spring.datasource.url")
        dataSource.username = env.getProperty("spring.datasource.username")
        dataSource.password = env.getProperty("spring.datasource.password")
        return dataSource
    }

    @Autowired
    @Bean("SoftwareDesign")
    @Throws(Exception::class)
    open fun getSessionFactory(dataSource: DataSource?): SessionFactory? {
        val properties = Properties()
        // See: application.properties
        properties["hibernate.dialect"] = env.getProperty("spring.jpa.properties.hibernate.dialect")
        properties["hibernate.show_sql"] = env.getProperty("spring.jpa.show-sql")
        properties["current_session_context_class"] =
            env.getProperty("spring.jpa.properties.hibernate.current_session_context_class")
        val factoryBean = LocalSessionFactoryBean()
        // Package contain entity classes
        factoryBean.setPackagesToScan("ru.akirakozov.sd.refactoring.entity")
        if (dataSource != null) {
            factoryBean.setDataSource(dataSource)
        }
        factoryBean.hibernateProperties = properties
        factoryBean.afterPropertiesSet()
        return factoryBean.getObject()
    }

    @Autowired
    @Bean("transactionManager")
    open fun getTransactionManager(sessionFactory: SessionFactory?): HibernateTransactionManager? {
        return sessionFactory?.let { HibernateTransactionManager(it) }
    }

    @Bean("ProductDao")
    open fun productDao(): ProductDao {
        return ProductDao()
    }

    override fun run(vararg args: String?) {
        val serverPortString = env.getProperty("server_port")
        val serverPort = serverPortString?.toInt() ?: error("Port is missing")

        val applicationServer = ApplicationServer(Server(serverPort), productDao())
        applicationServer.start()
    }
}

