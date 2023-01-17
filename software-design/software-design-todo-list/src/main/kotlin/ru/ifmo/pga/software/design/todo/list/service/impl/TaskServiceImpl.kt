package ru.ifmo.pga.software.design.todo.list.service.impl

import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.service.impl.NameDescriptionServiceImpl
import ru.ifmo.pga.software.design.todo.list.dao.TaskDao
import ru.ifmo.pga.software.design.todo.list.entity.Task
import ru.ifmo.pga.software.design.todo.list.entity.enums.Status
import ru.ifmo.pga.software.design.todo.list.service.TaskService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("taskService")
open class TaskServiceImpl : NameDescriptionServiceImpl<Task, TaskDao>(), TaskService {

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByTaskListId(id: Long): List<Task> {
        return invokeDaoMethod { dao.findByTaskListId(id) }
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findByTaskListIdAndStatus(id: Long, status: Status): List<Task> {
        return invokeDaoMethod { dao.findByTaskListIdAndStatus(id, status) }
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findNotDoneTasksByListId(id: Long): List<Task> {
        return findByTaskListIdAndStatus(id, Status.TO_DO)
    }

    @Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
    override fun findDoneTasksByListId(id: Long): List<Task> {
        return findByTaskListIdAndStatus(id, Status.DONE)
    }
}
