package ru.akirakozov.sd.refactoring.dao

import javax.persistence.EntityManager

interface Dao<T> {
    val entityManager: EntityManager

    fun findById(id: Long): T?

    fun findAll(): List<T>

    fun count(): Int

    fun save(entity: T): T

    fun remove(entity: T)

    fun remove(id: Long)
}