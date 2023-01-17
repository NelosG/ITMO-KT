package ru.ifmo.pga.software.design.todo.list.service

import ru.ifmo.pga.software.design.core.service.NameDescriptionService
import ru.ifmo.pga.software.design.todo.list.entity.Task
import ru.ifmo.pga.software.design.todo.list.entity.enums.Status

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
interface TaskService : NameDescriptionService<Task> {
    fun findByTaskListId(id: Long): List<Task>

    fun findByTaskListIdAndStatus(id: Long, status: Status): List<Task>

    fun findNotDoneTasksByListId(id: Long): List<Task>

    fun findDoneTasksByListId(id: Long): List<Task>
}
