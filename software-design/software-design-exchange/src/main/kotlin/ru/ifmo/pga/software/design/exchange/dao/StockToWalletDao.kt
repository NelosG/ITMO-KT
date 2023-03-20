package ru.ifmo.pga.software.design.exchange.dao

import ru.ifmo.pga.software.design.core.dao.Dao
import ru.ifmo.pga.software.design.exchange.entity.StockToWallet

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface StockToWalletDao : Dao<StockToWallet> {
    fun findByStockId(id: Long): List<StockToWallet>
    fun findByWalletId(id: Long): List<StockToWallet>
    fun findByStockIdAndWalletId(stockId: Long, walletId: Long): StockToWallet?
}
