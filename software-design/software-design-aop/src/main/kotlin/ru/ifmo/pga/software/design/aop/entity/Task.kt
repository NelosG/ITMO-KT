package ru.ifmo.pga.software.design.aop.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.aop.entity.enums.Status
import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = Task.TABLE_NAME)
@Table(name = Task.TABLE_NAME)
class Task : NameDescriptionEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "task_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    @get:Column(name = "status")
    var status: Status = Status.TO_DO

    companion object {
        const val STATUS = "status"

        const val TABLE_NAME = "task"
    }
}
