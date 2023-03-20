package ru.ifmo.pga.software.design.reactive.controller

import org.springframework.web.bind.annotation.*
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono
import ru.ifmo.pga.software.design.reactive.model.Currency
import ru.ifmo.pga.software.design.reactive.model.Product
import ru.ifmo.pga.software.design.reactive.model.ProductView
import ru.ifmo.pga.software.design.reactive.repository.ProductRepository
import ru.ifmo.pga.software.design.reactive.repository.UserRepository
import ru.ifmo.pga.software.design.reactive.service.CurrencyService
import java.math.BigDecimal
import java.util.UUID

@RestController
@RequestMapping("/products")
class ProductController(
    private val currencyService: CurrencyService,
    private val productRepository: ProductRepository,
    private val userRepository: UserRepository,
) {
    @PostMapping("/add")
    fun addProduct(userId: String, name: String, price: BigDecimal): Flux<Product> {
        return userRepository.findById(userId).flatMapMany { user ->
            return@flatMapMany productRepository.save(
                Product(
                    id = UUID.randomUUID().toString(),
                    name = name,
                    priceUsd = currencyService.convert(price, user.currency, Currency.USD),
                )
            )
        }
    }

    @GetMapping("/all")
    fun getAllProducts(userId: String): Flux<ProductView> {
        return userRepository.findById(userId).flatMapMany { user ->
            return@flatMapMany productRepository.findAll().map {
                ProductView(
                    name = it.name,
                    price = currencyService.convert(it.priceUsd, Currency.USD, user.currency)
                )
            }
        }
    }
}