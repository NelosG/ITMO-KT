package ru.ifmo.pga.software.design.todo.list.service.impl

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.context.annotation.Bean
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.service.impl.NameDescriptionServiceImpl
import ru.ifmo.pga.software.design.todo.list.dao.TaskListDao
import ru.ifmo.pga.software.design.todo.list.entity.TaskList
import ru.ifmo.pga.software.design.todo.list.service.TaskListService
import ru.ifmo.pga.software.design.todo.list.service.TaskService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("taskListService")
open class TaskListServiceImpl @Autowired constructor(
    private val taskService: TaskService
) : NameDescriptionServiceImpl<TaskList, TaskListDao>(), TaskListService {

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(id: Long) {
        taskService.findByTaskListId(id)
            .forEach {
                taskService.remove(it)
            }
        super.remove(id)
    }
}
