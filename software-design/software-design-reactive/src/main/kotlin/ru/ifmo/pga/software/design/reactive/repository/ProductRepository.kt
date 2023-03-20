package ru.ifmo.pga.software.design.reactive.repository

import org.springframework.data.repository.reactive.ReactiveCrudRepository
import org.springframework.stereotype.Repository
import ru.ifmo.pga.software.design.reactive.model.Product

@Repository
interface ProductRepository : ReactiveCrudRepository<Product, String>