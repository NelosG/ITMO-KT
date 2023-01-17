package ru.ifmo.pga.software.design.core.dao.impl

import jakarta.persistence.EntityManager
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.dao.Dao
import ru.ifmo.pga.software.design.core.dao.exceptions.DaoException
import ru.ifmo.pga.software.design.core.entity.AbstractEntity
import java.lang.reflect.ParameterizedType

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
abstract class DaoImpl<T : AbstractEntity> : Dao<T> {
    protected open lateinit var entityManager: EntityManager
    override val entityClass: Class<T>

    init {
        val genericSuperclass = javaClass.genericSuperclass as ParameterizedType
        val genericParameters = genericSuperclass.actualTypeArguments
        entityClass = genericParameters[0] as Class<T>
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findAll(): List<T> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(cq.select(root))
        return query.resultList
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findById(id: Long): T {
        return findEntityById(id, entityClass)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entity: T): T {
        return entityManager.merge(entity)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entities: Collection<T>): List<T> {
        val savedEntities = ArrayList<T>()
        entities.forEach { entity: T -> savedEntities.add(save(entity)) }
        return savedEntities
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(entity: T) {
        remove(entity.id ?: error("Missing id in entity for removal"))
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(id: Long) {
        val cb = entityManager.criteriaBuilder
        val cd = cb.createCriteriaDelete(entityClass)
        val root = cd.from(entityClass)
        cd.where(cb.equal(root.get<Any>("id"), cb.parameter(Long::class.java, "id")))
        entityManager
            .createQuery(cd)
            .setParameter("id", id)
            .executeUpdate()
    }

    private fun findEntityById(id: Long, entityClass: Class<T>): T {
        return entityManager.find(entityClass, id)
            ?: //TODO: add logs
            throw DaoException("Entity wasn't founded")
    }
}
