package ru.ifmo.pga.software.design.aop.dao.impl

import org.springframework.stereotype.Repository
import ru.ifmo.pga.software.design.aop.aspect.AspectProfile
import ru.ifmo.pga.software.design.aop.dao.TaskDao
import ru.ifmo.pga.software.design.aop.entity.Task
import ru.ifmo.pga.software.design.core.dao.impl.NameDescriptionDaoImpl

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("taskDao")
open class TaskDaoImpl : NameDescriptionDaoImpl<Task>(), TaskDao {
    @AspectProfile
    override fun save(entity: Task): Task {
        return super.save(entity)
    }
}
