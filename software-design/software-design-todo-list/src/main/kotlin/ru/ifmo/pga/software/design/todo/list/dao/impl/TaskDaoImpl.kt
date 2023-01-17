package ru.ifmo.pga.software.design.todo.list.dao.impl

import org.springframework.stereotype.Repository
import ru.ifmo.pga.software.design.core.dao.impl.NameDescriptionDaoImpl
import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity
import ru.ifmo.pga.software.design.todo.list.dao.TaskDao
import ru.ifmo.pga.software.design.todo.list.entity.Task
import ru.ifmo.pga.software.design.todo.list.entity.enums.Status

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("taskDao")
open class TaskDaoImpl : NameDescriptionDaoImpl<Task>(), TaskDao {

    override fun findByTaskListId(id: Long): List<Task> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.equal(
                        root.get<Long>(Task.TASK_LIST_ID),
                        id
                    )
                ).orderBy(
                    cb.asc(
                        root.get<String>(NameDescriptionEntity.NAME)
                    )
                )
        )
        return query.resultList
    }

    override fun findByTaskListIdAndStatus(id: Long, status: Status): List<Task> {
        val cb = entityManager.criteriaBuilder
        val cq = cb.createQuery(entityClass)
        val root = cq.from(entityClass)
        val query = entityManager.createQuery(
            cq.select(root)
                .where(
                    cb.and(
                        cb.equal(
                            root.get<Long>(Task.TASK_LIST_ID),
                            id
                        ),
                        cb.equal(
                            root.get<Long>(Task.STATUS),
                            status
                        )
                    )
                ).orderBy(
                    cb.asc(
                        root.get<String>(NameDescriptionEntity.NAME)
                    )
                )
        )
        return query.resultList
    }
}
