package ru.ifmo.pga.software.design.exchange.service

import ru.ifmo.pga.software.design.core.service.Service
import ru.ifmo.pga.software.design.exchange.entity.Stock
import ru.ifmo.pga.software.design.exchange.entity.StockToWallet

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface StockService : Service<Stock> {
    fun sellStock(userId: Long, stockId: Long, count: Long)
    fun buyStock(userId: Long, stockId: Long, count: Long): StockToWallet
    fun findByName(name: String): List<Stock>
    fun findByCompanyId(id: Long): List<Stock>
}
