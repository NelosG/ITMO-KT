package ru.ifmo.pga.software.design.core.service

import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface NameDescriptionService<T : NameDescriptionEntity> : Service<T> {
    fun findByName(name: String): List<T>
}
