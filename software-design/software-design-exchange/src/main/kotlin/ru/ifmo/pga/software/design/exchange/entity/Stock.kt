package ru.ifmo.pga.software.design.exchange.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.core.entity.AbstractEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = Stock.TABLE_NAME)
@Table(name = Stock.TABLE_NAME)
class Stock : AbstractEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "stock_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    @get:Column(name = "company_id")
    var companyId: Long? = null

    @get:Column(name = "name")
    var name: String? = null

    @get:Column(name = "price")
    var price: Long? = null

    companion object {
        const val COMPANY_ID = "companyId"
        const val NAME = "name"
        const val PRICE = "price"

        const val TABLE_NAME = "stock"
    }
}