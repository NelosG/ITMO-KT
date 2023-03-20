package ru.ifmo.pga.software.design.exchange.web

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import ru.ifmo.pga.software.design.exchange.entity.Company
import ru.ifmo.pga.software.design.exchange.entity.Stock
import ru.ifmo.pga.software.design.exchange.service.CompanyService
import ru.ifmo.pga.software.design.exchange.service.StockService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Controller
class CompanyController @Autowired constructor(
    private val companyService: CompanyService,
    private val stockService: StockService,
) {

    @RequestMapping(value = ["/companies"], method = [RequestMethod.GET])
    fun getCompanies(
        @RequestParam(name = "userId", required = true) userId: Long,
        model: Model
    ): String {
        model.addAttribute("companies", companyService.findAll())
        return "companies"
    }

    @RequestMapping(value = ["/companies/add-company"], method = [RequestMethod.POST])
    fun addCompany(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "name", required = true, defaultValue = "Some Company") name: String,
        @RequestParam(name = "description", required = false) description: String?,
    ): String {
        val company = companyService.save(Company().apply {
            this.name = name
            this.description = description
        })
        return "redirect:/company?userId=${userId}&id=${company.id}"
    }

    @RequestMapping(value = ["/company"], method = [RequestMethod.GET])
    fun getCompany(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        val company = companyService.findById(id) ?: error("Company not found")
        val stocks = stockService.findByCompanyId(id)
        model.addAttribute("company", company)
        model.addAttribute("stocks", stocks)
        return "company"
    }

    @RequestMapping(value = ["/company/add-stock"], method = [RequestMethod.POST])
    fun addStock(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "companyId", required = true) companyId: Long,
        @RequestParam(name = "name", required = true) name: String,
        @RequestParam(name = "price", required = true, defaultValue = 0.toString()) price: Long,
    ): String {
        val stock = stockService.save(Stock().apply {
            this.companyId = companyId
            this.name = name
            this.price = price
        })
        return "redirect:/stock?userId=${userId}&id=${stock.id}"
    }

    @RequestMapping(value = ["/company/delete-company"], method = [RequestMethod.POST])
    fun deleteCompany(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        companyService.remove(id)
        return "redirect:/companies?userId=${userId}"
    }
}
