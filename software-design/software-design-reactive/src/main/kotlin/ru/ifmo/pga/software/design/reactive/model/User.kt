package ru.ifmo.pga.software.design.reactive.model

import org.springframework.data.annotation.Id
import org.springframework.data.mongodb.core.mapping.Document
import ru.ifmo.pga.software.design.reactive.model.Currency

@Document("users")
data class User(
    @Id val id: String,
    val name: String,
    val currency: Currency
)