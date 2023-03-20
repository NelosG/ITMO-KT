package ru.ifmo.pga.software.design.exchange.dao.impl

import org.springframework.stereotype.Repository
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.dao.impl.GenericDaoImpl
import ru.ifmo.pga.software.design.exchange.dao.WalletDao
import ru.ifmo.pga.software.design.exchange.entity.Wallet

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("walletDao")
open class WalletDaoImpl : GenericDaoImpl<Wallet>(), WalletDao {

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByUserId(id: Long): Wallet? {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.equal(
                        root.get<Long>(Wallet.USER_ID),
                        id
                    )
                )
        )
        if (query.resultList.size > 1) error("Found multiple wallets")
        return query.resultList.firstOrNull()
    }
}