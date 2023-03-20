package ru.ifmo.pga.software.design.exchange.service.impl

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.service.impl.ServiceImpl
import ru.ifmo.pga.software.design.exchange.dao.StockToWalletDao
import ru.ifmo.pga.software.design.exchange.entity.StockToWallet
import ru.ifmo.pga.software.design.exchange.service.StockService
import ru.ifmo.pga.software.design.exchange.service.StockToWalletService
import ru.ifmo.pga.software.design.exchange.service.WalletService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("stockToWalletService")
open class StockToWalletServiceImpl @Autowired constructor(
    private val walletService: WalletService
) : ServiceImpl<StockToWallet, StockToWalletDao>(), StockToWalletService {

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByStockId(id: Long): List<StockToWallet> {
        return dao.findByStockId(id)
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByWalletId(id: Long): List<StockToWallet> {
        return dao.findByWalletId(id)
    }

    override fun findByUserId(id: Long): List<StockToWallet> {
        val wallet = walletService.findByUserId(id) ?: error("Wallet not found")
        val walletId = wallet.id ?: error("Wallet id is null")
        return findByWalletId(walletId)
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByStockIdAndWalletId(stockId: Long, walletId: Long): StockToWallet? {
        return dao.findByStockIdAndWalletId(stockId, walletId)
    }
}
