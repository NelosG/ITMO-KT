package ru.ifmo.pga.software.design.core.dao.impl

import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.dao.NameDescriptionDao
import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
abstract class NameDescriptionDaoImpl<T : NameDescriptionEntity> : GenericDaoImpl<T>(), NameDescriptionDao<T> {

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByName(name: String): List<T> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.equal(
                        root.get<String>(NameDescriptionEntity.NAME),
                        name
                    )
                ).orderBy(
                    cb.asc(
                        root.get<String>(NameDescriptionEntity.NAME)
                    )
                )
        )
        return query.resultList
    }
}
