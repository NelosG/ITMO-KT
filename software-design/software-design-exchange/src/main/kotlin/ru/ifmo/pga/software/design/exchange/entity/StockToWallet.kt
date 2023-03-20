package ru.ifmo.pga.software.design.exchange.entity

import jakarta.persistence.*
import ru.ifmo.pga.software.design.core.entity.AbstractEntity

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Entity(name = StockToWallet.TABLE_NAME)
@Table(name = StockToWallet.TABLE_NAME)
class StockToWallet : AbstractEntity() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "stocks_wallet_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.IDENTITY, generator = "seq_gen")
    @Column(name = "id", nullable = false)
    override fun getId(): Long? = id

    @get:Column(name = "stock_id")
    var stockId: Long? = null

    @get:Column(name = "wallet_id")
    var walletId: Long? = null

    @get:Column(name = "count")
    var count: Long? = null

    companion object {
        const val STOCK_ID = "stockId"
        const val WALLET_ID = "walletId"
        const val COUNT = "count"

        const val TABLE_NAME = "stocks_wallet"
    }
}