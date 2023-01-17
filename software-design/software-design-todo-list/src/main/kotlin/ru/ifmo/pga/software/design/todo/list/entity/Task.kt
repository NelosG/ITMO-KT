package ru.ifmo.pga.software.design.todo.list.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity
import ru.ifmo.pga.software.design.todo.list.entity.enums.Status

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = Task.TABLE_NAME)
@Table(name = Task.TABLE_NAME)
class Task : NameDescriptionEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "task_seq", allocationSize = 1) //TODO:
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    @get:Column(name = "task_list_id")
    var taskListId: Long? = null

    @get:Column(name = "status")
    var status: Status = Status.TO_DO

    companion object {
        const val TASK_LIST_ID = "taskListId"
        const val STATUS = "status"

        const val TABLE_NAME = "task"
    }
}
