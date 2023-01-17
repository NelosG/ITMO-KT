package ru.ifmo.pga.software.design.todo.list.dao

import ru.ifmo.pga.software.design.core.dao.NameDescriptionDao
import ru.ifmo.pga.software.design.todo.list.entity.Task
import ru.ifmo.pga.software.design.todo.list.entity.enums.Status

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface TaskDao : NameDescriptionDao<Task> {
    fun findByTaskListId(id: Long): List<Task>
    fun findByTaskListIdAndStatus(id: Long, status: Status): List<Task>
}
