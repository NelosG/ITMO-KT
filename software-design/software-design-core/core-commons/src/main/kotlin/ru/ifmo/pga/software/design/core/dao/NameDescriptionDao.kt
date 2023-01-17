package ru.ifmo.pga.software.design.core.dao

import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface NameDescriptionDao<T : NameDescriptionEntity> : Dao<T> {
    fun findByName(name: String): List<T>
}
