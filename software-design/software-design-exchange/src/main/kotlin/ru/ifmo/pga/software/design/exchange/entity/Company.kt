package ru.ifmo.pga.software.design.exchange.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.core.entity.NameDescriptionEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = Company.TABLE_NAME)
@Table(name = Company.TABLE_NAME)
class Company : NameDescriptionEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "company_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    companion object {
        const val TABLE_NAME = "company"
    }
}