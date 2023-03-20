package ru.ifmo.pga.software.design.exchange.service.impl

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.service.impl.ServiceImpl
import ru.ifmo.pga.software.design.exchange.dao.StockDao
import ru.ifmo.pga.software.design.exchange.entity.Stock
import ru.ifmo.pga.software.design.exchange.entity.StockToWallet
import ru.ifmo.pga.software.design.exchange.service.StockService
import ru.ifmo.pga.software.design.exchange.service.StockToWalletService
import ru.ifmo.pga.software.design.exchange.service.WalletService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("stockService")
open class StockServiceImpl @Autowired constructor(
    private val walletService: WalletService,
    private val stockToWalletService: StockToWalletService
) : ServiceImpl<Stock, StockDao>(), StockService {

    @Transactional(propagation = Propagation.REQUIRED)
    override fun sellStock(userId: Long, stockId: Long, count: Long) {
        val wallet = walletService.findByUserId(userId) ?: error("Wallet not found")
        val stock = findById(stockId) ?: error("Stock not found")
        val stockToWallet = stockToWalletService.findByStockIdAndWalletId(walletId = wallet.id!!, stockId = stock.id!!)
            ?: error("StockToWallet not found")
        when {
            stockToWallet.count == count -> {
                stockToWalletService.remove(stockToWallet)
            }

            stockToWallet.count!! > count -> {
                stockToWallet.count = stockToWallet.count!! - count
            }

            else -> error("Not enough stocks")
        }

        wallet.amount = wallet.amount!! + stock.price!! * count
        walletService.save(wallet)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun buyStock(userId: Long, stockId: Long, count: Long): StockToWallet {
        val wallet = walletService.findByUserId(userId) ?: error("Wallet not found")
        val stock = findById(stockId)
        if (wallet.amount!! < stock.price!! * count) error("Not enough money")
        wallet.amount = wallet.amount!! - stock.price!! * count
        walletService.save(wallet)

        val stockToWallet = stockToWalletService.findByStockIdAndWalletId(walletId = wallet.id!!, stockId = stock.id!!) ?: StockToWallet()

        return stockToWalletService.save(stockToWallet.apply {
            this.walletId = wallet.id
            this.stockId = stockId
            this.count = (this.count ?: 0) + count
        })
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(id: Long) {
        stockToWalletService.findByStockId(id)
            .forEach {
                stockToWalletService.remove(it)
            }
        super.remove(id)
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByName(name: String): List<Stock> {
        return dao.findByName(name)
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByCompanyId(id: Long): List<Stock> {
        return dao.findByCompanyId(id)
    }
}