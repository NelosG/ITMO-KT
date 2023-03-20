package ru.ifmo.pga.software.design.exchange.service

import ru.ifmo.pga.software.design.core.service.Service
import ru.ifmo.pga.software.design.exchange.entity.StockToWallet

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface StockToWalletService : Service<StockToWallet> {
    fun findByStockId(id: Long): List<StockToWallet>
    fun findByWalletId(id: Long): List<StockToWallet>
    fun findByUserId(id: Long): List<StockToWallet>
    fun findByStockIdAndWalletId(stockId: Long, walletId: Long): StockToWallet?
}
