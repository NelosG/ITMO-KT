package ru.ifmo.pga.software.design.exchange.service.impl

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.service.impl.NameDescriptionServiceImpl
import ru.ifmo.pga.software.design.exchange.dao.CompanyDao
import ru.ifmo.pga.software.design.exchange.entity.Company
import ru.ifmo.pga.software.design.exchange.service.CompanyService
import ru.ifmo.pga.software.design.exchange.service.StockService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("companyService")
open class CompanyServiceImpl @Autowired constructor(
    private val stockService: StockService
) : NameDescriptionServiceImpl<Company, CompanyDao>(), CompanyService {

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(id: Long) {
        stockService.findByCompanyId(id)
            .forEach {
                stockService.remove(it)
            }
        super.remove(id)
    }
}
