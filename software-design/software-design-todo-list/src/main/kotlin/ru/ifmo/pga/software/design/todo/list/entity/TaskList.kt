package ru.ifmo.pga.software.design.todo.list.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = TaskList.TABLE_NAME)
@Table(name = TaskList.TABLE_NAME)
class TaskList : NameDescriptionEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "task_list_seq", allocationSize = 1) //TODO:
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    companion object {
        const val TABLE_NAME = "task_list"
    }
}
