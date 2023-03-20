package ru.ifmo.pga.software.design.exchange

import org.junit.BeforeClass
import org.junit.Test
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.BeforeEach
import org.junit.runner.RunWith
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.TestPropertySource
import org.springframework.test.context.junit4.SpringRunner
import org.springframework.web.client.RestTemplate
import org.springframework.web.client.postForObject
import org.springframework.web.util.UriComponentsBuilder
import org.testcontainers.containers.FixedHostPortGenericContainer
import org.testcontainers.containers.GenericContainer
import org.testcontainers.junit.jupiter.Container
import org.testcontainers.junit.jupiter.Testcontainers
import ru.ifmo.pga.software.design.exchange.Configuration.Companion.DB_NAME
import ru.ifmo.pga.software.design.exchange.Configuration.Companion.DB_PORT
import ru.ifmo.pga.software.design.exchange.Configuration.Companion.PASSWORD
import ru.ifmo.pga.software.design.exchange.Configuration.Companion.USERNAME
import ru.ifmo.pga.software.design.exchange.entity.Company
import ru.ifmo.pga.software.design.exchange.entity.Stock
import ru.ifmo.pga.software.design.exchange.entity.StockToWallet
import ru.ifmo.pga.software.design.exchange.entity.User
import ru.ifmo.pga.software.design.exchange.service.*
import java.util.*


/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 *
 * Поднимается 2 контейнера(приложение и бд)
 * + локально поднимаются только сервисы для работы с бд (не хочется рукописными запросами смотреть что в бд)
 */
@Testcontainers
@RunWith(SpringRunner::class)
@SpringBootTest(
    webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, classes = [Configuration::class]
)
@TestPropertySource(locations = ["classpath:application-integrationtest.properties"])
class IntegrationTest {


    @Autowired
    private lateinit var companyService: CompanyService

    @Autowired
    private lateinit var stockService: StockService

    @Autowired
    private lateinit var stockToWalletService: StockToWalletService

    @Autowired
    private lateinit var userService: UserService

    @Autowired
    private lateinit var walletService: WalletService

    private lateinit var admin: User


    @BeforeEach
    fun init() {
        admin = userService.findById(ADMIN_USER_ID)!!
    }

    private val restTemplate = RestTemplate()

    @Test
    fun createUserTest() {
        val initialUsers = userService.findAll()
        val initialWalletsSize = walletService.findAll().size

        post("/login/sign-up")
        val newUsers = userService.findAll().filter { !initialUsers.contains(it) }
        val allWallets = walletService.findAll()
        assertEquals(initialWalletsSize + 1, allWallets.size)
        assertEquals(1, newUsers.size)

        val newWallets = allWallets.filter { it.userId == newUsers.first().id }
        assertEquals(1, newWallets.size)
        assertEquals(newUsers.first().id, newWallets.first().userId)
    }

    @Test
    fun addMoneyTest() {
        val wallet = walletService.findByUserId(ADMIN_USER_ID)!!
        val amount = 10L
        post(
            "/wallet/add-money",
            "userId" to ADMIN_USER_ID.toString(),
            "walletId" to wallet.id.toString(),
            "amount" to "10",
        )

        val newWallet = walletService.findByUserId(ADMIN_USER_ID)!!
        assertEquals(amount, newWallet.amount?.minus(wallet.amount!!))
    }

    @Test
    fun createCompanyTest() {
        val initialCompanies = companyService.findAll()
        val name = "createCompanyTest"
        val description = "description"
        post(
            "/companies/add-company",
            "userId" to ADMIN_USER_ID.toString(),
            "name" to name,
            "description" to description,
        )
        val newCompanies = companyService.findAll().filter { !initialCompanies.contains(it) }
        assertEquals(1, newCompanies.size)
        assertEquals(name, newCompanies.first().name)
        assertEquals(description, newCompanies.first().description)
    }

    @Test
    fun deleteCompanyTest() {
        val name = "deleteCompanyTest"
        val company = companyService.save(Company().apply { this.name = name })
        post(
            "/company/delete-company",
            "userId" to ADMIN_USER_ID.toString(),
            "id" to company.id.toString(),
        )
        val emptyCompaniesList = companyService.findByName(name)
        assertEquals(0, emptyCompaniesList.size)
    }

    @Test
    fun createStockTest() {
        val companyName = "createStockTest"
        val stockName = "stockName"
        val price = 666L
        val initialStocks = stockService.findAll()
        val company = companyService.save(Company().apply { this.name = companyName })

        post(
            "/company/add-stock",
            "userId" to ADMIN_USER_ID.toString(),
            "companyId" to company.id.toString(),
            "name" to stockName,
            "price" to price.toString(),
        )

        val newStocks = stockService.findAll().filter { !initialStocks.contains(it) }
        assertEquals(1, newStocks.size)
        assertEquals(newStocks.first().name, stockName)
        assertEquals(price, newStocks.first().price)
    }

    @Test
    fun updatePriceStockTest() {
        val companyName = "updatePriceStockTest"
        val stockName = "stockName"
        val price = 666L
        val newPrice = 123L
        val company = companyService.save(Company().apply { this.name = companyName })
        val stock = stockService.save(Stock().apply {
            this.name = stockName
            this.companyId = company.id
            this.price = price
        })
        post(
            "/stock/change-price",
            "userId" to ADMIN_USER_ID.toString(),
            "stockId" to stock.id.toString(),
            "price" to newPrice.toString(),
        )
        val updatedStock = stockService.findByCompanyId(company.id!!).first()
        assertEquals(newPrice, updatedStock.price)
    }

    @Test
    fun deleteStockTest() {
        val companyName = "deleteStockTest"
        val stockName = "stockName"
        val price = 666L
        val company = companyService.save(Company().apply { this.name = companyName })
        val stock = stockService.save(Stock().apply {
            this.name = stockName
            this.companyId = company.id
            this.price = price
        })
        post(
            "/stock/delete-stock",
            "userId" to ADMIN_USER_ID.toString(),
            "id" to stock.id.toString(),
        )
        // у этой компании была только 1 акция
        val emptyStocksList = stockService.findByCompanyId(company.id!!)
        assertEquals(0, emptyStocksList.size)
    }

    @Test
    fun buyStockTest() {
        val companyName = "buyStockTest"
        val stockName = "stockName"
        val price = 500L
        val count = 2L
        val amount = 1000L
        val user = userService.save(User())
        val wallet = walletService.findByUserId(user.id!!)!!
        wallet.amount = amount
        walletService.save(wallet)
        val company = companyService.save(Company().apply { this.name = companyName })
        val stock = stockService.save(Stock().apply {
            this.name = stockName
            this.companyId = company.id
            this.price = price
        })

        post(
            "/user/buy-stock",
            "userId" to user.id.toString(),
            "stockId" to stock.id.toString(),
            "count" to count.toString(),
        )
        val stockToWallet = stockToWalletService.findByStockIdAndWalletId(stock.id!!, wallet.id!!)
        val updatedWallet = walletService.findByUserId(user.id!!)!!
        assertEquals(amount - price * count, updatedWallet.amount)
        assertEquals(count, stockToWallet?.count)
    }


    @Test
    fun sellStockTest() {
        val companyName = "buyStockTest"
        val stockName = "stockName"
        val price = 500L
        val initialCount = 3L
        val count = 2L
        val user = userService.save(User())
        val wallet = walletService.findByUserId(user.id!!)!!
        val company = companyService.save(Company().apply { this.name = companyName })
        val stock = stockService.save(Stock().apply {
            this.name = stockName
            this.companyId = company.id
            this.price = price
        })
        stockToWalletService.save(StockToWallet().apply {
            stockId = stock.id
            walletId = wallet.id
            this.count = initialCount
        })

        post(
            "/user/sell-stock",
            "userId" to user.id.toString(),
            "stockId" to stock.id.toString(),
            "count" to count.toString(),
        )
        val stockToWallet = stockToWalletService.findByStockIdAndWalletId(stock.id!!, wallet.id!!)
        val updatedWallet = walletService.findByUserId(user.id!!)!!
        assertEquals(price * count, updatedWallet.amount)
        assertEquals(initialCount - count, stockToWallet?.count)
    }

    @Test
    fun sellAllStockTest() {
        val companyName = "buyStockTest"
        val stockName = "stockName"
        val price = 500L
        val count = 2L
        val user = userService.save(User())
        val wallet = walletService.findByUserId(user.id!!)!!
        val company = companyService.save(Company().apply { this.name = companyName })
        val stock = stockService.save(Stock().apply {
            this.name = stockName
            this.companyId = company.id
            this.price = price
        })
        stockToWalletService.save(StockToWallet().apply {
            stockId = stock.id
            walletId = wallet.id
            this.count = count
        })

        post(
            "/user/sell-stock",
            "userId" to user.id.toString(),
            "stockId" to stock.id.toString(),
            "count" to count.toString(),
        )
        val stockToWallet = stockToWalletService.findByStockIdAndWalletId(stock.id!!, wallet.id!!)
        val updatedWallet = walletService.findByUserId(user.id!!)!!
        assertEquals(price * count, updatedWallet.amount)
        assertNull(stockToWallet)
    }


    private fun post(pathSegment: String, vararg params: Pair<String, String>) {
        val path = if (pathSegment[0] == '/') pathSegment.substring(1) else pathSegment
        val uriBuilder = UriComponentsBuilder.fromUriString(URI).pathSegment(path)

        for (param in params) {
            uriBuilder.queryParam(param.first, param.second)
        }
        val uri = uriBuilder.build().toUri()
        restTemplate.postForObject<Any?>(uri, null)
    }

    companion object {
        private const val URI = "http://localhost:8080"

        private const val ADMIN_USER_ID = 0L


        @Container
        var exchangeContainer: GenericContainer<*> =
            FixedHostPortGenericContainer("exchange:1.0.0-SNAPSHOT").withFixedExposedPort(8080, 8080)

        @Container
        var postgresContainer: GenericContainer<*> =
            FixedHostPortGenericContainer("postgres:14").withFixedExposedPort(DB_PORT, 5432)
                .withEnv("POSTGRES_DB", DB_NAME).withEnv("POSTGRES_PASSWORD", PASSWORD)
                .withEnv("POSTGRES_USER", USERNAME)

        @JvmStatic
        @BeforeClass
        fun mainSetUp() {
            exchangeContainer.start()
            postgresContainer.start()
        }
    }
}