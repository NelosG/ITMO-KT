package ru.ifmo.pga.software.design.exchange.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.core.entity.AbstractEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = User.TABLE_NAME)
@Table(name = User.TABLE_NAME)
class User : AbstractEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "users_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    companion object {
        const val TABLE_NAME = "users"
    }
}