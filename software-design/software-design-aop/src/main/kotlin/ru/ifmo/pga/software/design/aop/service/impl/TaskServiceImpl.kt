package ru.ifmo.pga.software.design.aop.service.impl

import org.springframework.stereotype.Service
import ru.ifmo.pga.software.design.aop.aspect.AspectProfile
import ru.ifmo.pga.software.design.aop.dao.TaskDao
import ru.ifmo.pga.software.design.aop.entity.Task
import ru.ifmo.pga.software.design.aop.service.TaskService
import ru.ifmo.pga.software.design.core.service.impl.NameDescriptionServiceImpl

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("taskService")
open class TaskServiceImpl : NameDescriptionServiceImpl<Task, TaskDao>(), TaskService {


    override fun remove(entities: Collection<Task>) {
        super.remove(entities)
    }

    @AspectProfile
    override fun save(entities: Collection<Task>): List<Task> {
        val savedEntities = ArrayList<Task>()
        for (entity in entities) {
            savedEntities.add(save(entity))
        }
        return savedEntities
    }
}
