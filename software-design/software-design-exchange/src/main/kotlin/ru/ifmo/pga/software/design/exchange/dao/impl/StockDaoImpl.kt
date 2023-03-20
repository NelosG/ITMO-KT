package ru.ifmo.pga.software.design.exchange.dao.impl

import org.springframework.stereotype.Repository
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.dao.impl.GenericDaoImpl
import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity
import ru.ifmo.pga.software.design.exchange.dao.StockDao
import ru.ifmo.pga.software.design.exchange.entity.Stock

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("stockDao")
open class StockDaoImpl : GenericDaoImpl<Stock>(), StockDao {

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByName(name: String): List<Stock> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.equal(
                        root.get<String>(Stock.NAME),
                        name
                    )
                ).orderBy(
                    cb.asc(
                        root.get<String>(Stock.NAME)
                    )
                )
        )
        return query.resultList
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByCompanyId(id: Long): List<Stock> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.equal(
                        root.get<Long>(Stock.COMPANY_ID),
                        id
                    )
                ).orderBy(
                    cb.desc(
                        root.get<Long>(Stock.PRICE)
                    )
                )
        )
        return query.resultList
    }
}