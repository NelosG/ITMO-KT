package ru.ifmo.pga.software.design.exchange.dao

import ru.ifmo.pga.software.design.core.dao.Dao
import ru.ifmo.pga.software.design.exchange.entity.Stock

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface StockDao : Dao<Stock> {
    fun findByName(name: String): List<Stock>
    fun findByCompanyId(id: Long): List<Stock>
}
