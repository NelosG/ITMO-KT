package ru.ifmo.pga.software.design.exchange.service.impl

import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.service.impl.ServiceImpl
import ru.ifmo.pga.software.design.exchange.dao.WalletDao
import ru.ifmo.pga.software.design.exchange.entity.Wallet
import ru.ifmo.pga.software.design.exchange.service.WalletService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("walletService")
open class WalletServiceImpl : ServiceImpl<Wallet, WalletDao>(), WalletService {

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entity: Wallet): Wallet {
        entity.amount = entity.amount ?: 0L
        return super.save(entity)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entities: Collection<Wallet>): List<Wallet> {
        val toSave = entities.map { entity ->
            entity.amount = entity.amount ?: 0L
            entity
        }
        return super.save(toSave)
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByUserId(id: Long): Wallet? {
        return dao.findByUserId(id)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun addMoney(id: Long, amount: Long) {
        val wallet = findById(id)
        wallet.amount = wallet.amount!! + amount
        save(wallet)
    }
}
