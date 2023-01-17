package ru.akirakozov.sd.refactoring.dao

import org.springframework.stereotype.Repository
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.akirakozov.sd.refactoring.entity.Product
import java.util.*

@Repository("productDaoRep")
open class ProductDao : AbstractDao<Product>() {

    @Transactional(propagation = Propagation.REQUIRED)
    override fun findById(id: Long): Product? {
        return entityManager.find(Product::class.java, id)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun findAll(): List<Product> {
        val cb = entityManager.criteriaBuilder
        val cqProduct = cb.createQuery(Product::class.java)
        val rootProduct = cqProduct.from(Product::class.java)
        val query = entityManager.createQuery(cqProduct.select(rootProduct))
        return query.resultList
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(id: Long) {
        entityManager.remove(findById(id))
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(entity: Product) {
        entityManager.remove(entity)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entity: Product): Product {
        return entityManager.merge(entity)
    }

    override fun count(): Int {
        return findAll().count()
    }

    fun findSum(): Long {
        val cb = entityManager.criteriaBuilder
        val cqProduct = cb.createQuery(Long::class.java)
        val rootProduct = cqProduct.from(Product::class.java)
        val query = cqProduct.select(entityManager.criteriaBuilder.sum(rootProduct.get("price")))
        return entityManager.createQuery(query).singleResult
    }

    fun findMax(): Optional<Product> {
        val cb = entityManager.criteriaBuilder
        val cqProduct = cb.createQuery(Product::class.java)
        val rootProduct = cqProduct.from(Product::class.java)
        val res = entityManager.createQuery(
            cqProduct.select(rootProduct).orderBy(
                cb.desc(
                    rootProduct.get<Long>("price")
                )
            )
        ).setMaxResults(1).resultList
        return Optional.ofNullable(res.firstOrNull())
    }

    fun findMin(): Optional<Product> {
        val cb = entityManager.criteriaBuilder
        val cqProduct = cb.createQuery(Product::class.java)
        val rootProduct = cqProduct.from(Product::class.java)
        val res = entityManager.createQuery(
            cqProduct.select(rootProduct).orderBy(
                cb.asc(
                    rootProduct.get<Long>("price")
                )
            )
        ).setMaxResults(1).resultList
        return Optional.ofNullable(res.firstOrNull())
    }
}