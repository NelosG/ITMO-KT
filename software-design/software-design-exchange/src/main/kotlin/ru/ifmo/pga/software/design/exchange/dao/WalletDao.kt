package ru.ifmo.pga.software.design.exchange.dao

import ru.ifmo.pga.software.design.core.dao.Dao
import ru.ifmo.pga.software.design.exchange.entity.Wallet

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface WalletDao : Dao<Wallet> {
    fun findByUserId(id: Long): Wallet?
}
