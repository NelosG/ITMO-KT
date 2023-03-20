package ru.ifmo.pga.software.design.reactive.controller

import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController
import reactor.core.publisher.Mono
import ru.ifmo.pga.software.design.reactive.model.Currency
import ru.ifmo.pga.software.design.reactive.model.User
import ru.ifmo.pga.software.design.reactive.repository.UserRepository
import java.util.UUID

@RestController
@RequestMapping("/users")
class UserController(private val userRepository: UserRepository) {
    @PostMapping("/register")
    fun register(name: String, currency: Currency): Mono<User> {
        return userRepository.save(
            User(
            id = UUID.randomUUID().toString(),
            name = name,
            currency = currency
        )
        )
    }
}