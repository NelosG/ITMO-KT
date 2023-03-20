package ru.ifmo.pga.software.design.exchange.service

import ru.ifmo.pga.software.design.core.service.Service
import ru.ifmo.pga.software.design.exchange.entity.User

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface UserService : Service<User> {
    fun isAdmin(id:Long): Boolean
    fun isAdmin(user:User): Boolean
}
