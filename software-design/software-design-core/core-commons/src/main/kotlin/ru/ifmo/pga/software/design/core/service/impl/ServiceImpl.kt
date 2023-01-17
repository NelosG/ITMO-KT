package ru.ifmo.pga.software.design.core.service.impl

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.dao.Dao
import ru.ifmo.pga.software.design.core.entity.AbstractEntity
import ru.ifmo.pga.software.design.core.service.Service
import ru.ifmo.pga.software.design.core.service.exceptions.ServiceException

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
abstract class ServiceImpl<T : AbstractEntity, DAO : Dao<T>> :
    Service<T> {

    @Autowired
    protected lateinit var dao: DAO

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findAll(): List<T> {
        return invokeDaoMethod { dao.findAll() }
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findById(id: Long): T {
        return invokeDaoMethod { dao.findById(id) }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entity: T): T {
        return invokeDaoMethod { dao.save(entity) }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entities: Collection<T>): List<T> {
        return invokeDaoMethod { dao.save(entities) }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(entity: T) {
        remove(entity.id)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(entities: Collection<T>) {
        return entities.forEach(::remove)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(ids: List<Long>) {
        return ids.forEach(::remove)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(id: Long) {
        try {
            dao.remove(id)
        } catch (e: Exception) {
            throw createDataAccessError(e)
        }
    }

    fun <R> invokeDaoMethod(function: Function1<DAO, R>): R {
        return try {
            function.invoke(dao)
        } catch (e: Exception) {
            throw createDataAccessError(e)
        }
    }

    private fun createDataAccessError(e: Exception): ServiceException {
        //TODO: add logs
        return ServiceException("Failed to access DB: ${e.message}", e)
    }

}
