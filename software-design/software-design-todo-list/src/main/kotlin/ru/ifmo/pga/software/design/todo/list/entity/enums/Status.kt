package ru.ifmo.pga.software.design.todo.list.entity.enums

import jakarta.persistence.Converter
import ru.ifmo.pga.software.design.core.entity.enums.JpaEnum
import ru.ifmo.pga.software.design.core.entity.enums.JpaEnumConverter

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
enum class Status(private val dbKey: String) : JpaEnum {
    TO_DO("D"),
    DONE("F");

    override fun getDbKey(): String {
        return dbKey
    }

    @Converter(autoApply = true)
    class JpaConverter : JpaEnumConverter<Status>()
}
