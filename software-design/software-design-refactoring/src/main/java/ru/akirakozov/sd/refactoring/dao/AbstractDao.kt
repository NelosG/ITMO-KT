package ru.akirakozov.sd.refactoring.dao

import javax.persistence.EntityManager
import javax.persistence.PersistenceContext

abstract class AbstractDao<T> : Dao<T> {
    @PersistenceContext(unitName = "SoftwareDesign")
    override lateinit var entityManager: EntityManager
}