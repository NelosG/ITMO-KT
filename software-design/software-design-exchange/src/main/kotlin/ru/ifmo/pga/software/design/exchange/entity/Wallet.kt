package ru.ifmo.pga.software.design.exchange.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.core.entity.AbstractEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = Wallet.TABLE_NAME)
@Table(name = Wallet.TABLE_NAME)
class Wallet : AbstractEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "wallet_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    @get:Column(name = "user_id")
    var userId: Long? = null

    @get:Column(name = "amount")
    var amount: Long? = null

    companion object {
        const val USER_ID = "userId"
        const val AMOUNT = "amount"

        const val TABLE_NAME = "wallet"
    }
}