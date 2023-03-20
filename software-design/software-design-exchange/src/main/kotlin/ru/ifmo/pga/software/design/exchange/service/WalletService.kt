package ru.ifmo.pga.software.design.exchange.service

import ru.ifmo.pga.software.design.core.service.Service
import ru.ifmo.pga.software.design.exchange.entity.Wallet

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface WalletService : Service<Wallet> {
    fun findByUserId(id: Long): Wallet?
    fun addMoney(id:Long, amount:Long)
}
