package ru.ifmo.pga.software.design.exchange.dao.impl

import org.springframework.stereotype.Repository
import ru.ifmo.pga.software.design.core.dao.impl.NameDescriptionDaoImpl
import ru.ifmo.pga.software.design.exchange.dao.CompanyDao
import ru.ifmo.pga.software.design.exchange.entity.Company

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("companyDao")
open class CompanyDaoImpl : NameDescriptionDaoImpl<Company>(), CompanyDao