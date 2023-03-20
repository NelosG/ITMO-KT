package ru.ifmo.pga.software.design.reactive.model

import org.springframework.data.annotation.Id
import org.springframework.data.mongodb.core.mapping.Document
import java.math.BigDecimal

@Document("products")
data class Product(
    @Id val id: String,
    val name: String,
    val priceUsd: BigDecimal,
)