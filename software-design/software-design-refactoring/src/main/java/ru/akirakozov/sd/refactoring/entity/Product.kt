package ru.akirakozov.sd.refactoring.entity

import java.util.*
import javax.persistence.*

@Entity(name = "Product")
@Table(name = Product.TABLE_NAME)
class Product() {

    @Id
    @SequenceGenerator(name = "seq_gen", sequenceName = "product_id_seq", allocationSize = 1)
    @GeneratedValue(strategy = GenerationType.AUTO, generator = "product_id_seq")
    @Column(name = "id", nullable = false)
    var id: Int? = null

    @Column(name = "name", nullable = false)
    private var name: String? = null

    fun getName(): String {
        return name ?: error("Name missing")
    }


    @Column(name = "price", nullable = false)
    private var price: Long? = null

    fun getPrice(): Long {
        return price ?: error("Price missing")
    }

    constructor(name: String, price: Long) : this() {
        this.name = name
        this.price = price
    }

    constructor(id: Int, name: String, price: Long) : this(name, price) {
        this.id = id
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other is Product) {
            return id == other.id && price == other.price && name == other.name
        }
        return false
    }

    override fun hashCode(): Int {
        return Objects.hash(id, name, price)
    }

    override fun toString(): String {
        return "Product {" +
                "id=" + id +
                ", name='" + name + "'" +
                ", price=" + price +
                '}'
    }

    companion object {
        const val TABLE_NAME = "product"
    }
}