package ru.ifmo.pga.software.design.exchange.dao.impl

import org.springframework.stereotype.Repository
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.dao.impl.GenericDaoImpl
import ru.ifmo.pga.software.design.exchange.dao.StockToWalletDao
import ru.ifmo.pga.software.design.exchange.entity.StockToWallet

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("stockToWalletDao")
open class StockToWalletDaoImpl : GenericDaoImpl<StockToWallet>(), StockToWalletDao {

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByStockId(id: Long): List<StockToWallet> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.equal(
                        root.get<Long>(StockToWallet.STOCK_ID),
                        id
                    )
                ).orderBy(
                    cb.desc(
                        root.get<Long>(StockToWallet.COUNT)
                    )
                )
        )
        return query.resultList
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByWalletId(id: Long): List<StockToWallet> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.equal(
                        root.get<Long>(StockToWallet.WALLET_ID),
                        id
                    )
                ).orderBy(
                    cb.desc(
                        root.get<Long>(StockToWallet.COUNT)
                    )
                )
        )
        return query.resultList
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByStockIdAndWalletId(stockId: Long, walletId: Long): StockToWallet? {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.and(
                        cb.equal(
                            root.get<Long>(StockToWallet.STOCK_ID),
                            stockId
                        ),
                        cb.equal(
                            root.get<Long>(StockToWallet.WALLET_ID),
                            walletId
                        )
                    )
                ).orderBy(
                    cb.desc(
                        root.get<Long>(StockToWallet.COUNT)
                    )
                )
        )
        return query.resultList.firstOrNull()
    }
}