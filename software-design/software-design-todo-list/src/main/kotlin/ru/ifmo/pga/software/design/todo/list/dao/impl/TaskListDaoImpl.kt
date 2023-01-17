package ru.ifmo.pga.software.design.todo.list.dao.impl

import org.springframework.stereotype.Repository
import ru.ifmo.pga.software.design.core.dao.impl.NameDescriptionDaoImpl
import ru.ifmo.pga.software.design.todo.list.dao.TaskListDao
import ru.ifmo.pga.software.design.todo.list.entity.TaskList

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("taskListDao")
open class TaskListDaoImpl : NameDescriptionDaoImpl<TaskList>(), TaskListDao
