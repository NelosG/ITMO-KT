package ru.ifmo.pga.software.design.exchange.web

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import ru.ifmo.pga.software.design.exchange.service.CompanyService
import ru.ifmo.pga.software.design.exchange.service.StockService
import ru.ifmo.pga.software.design.exchange.service.StockToWalletService
import ru.ifmo.pga.software.design.exchange.service.UserService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Controller
class StockController @Autowired constructor(
    private val stockService: StockService,
    private val companyService: CompanyService,
    private val userService: UserService,
) {
    @RequestMapping(value = ["/stock"], method = [RequestMethod.GET])
    fun getStock(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        val stock = stockService.findById(id) ?: error("Stock not found")
        val company = companyService.findById(stock.companyId!!) ?: error("Company not found")
        val isAdmin = userService.isAdmin(userId)
        model.addAttribute("stock", stock)
        model.addAttribute("companyName", company.name)
        model.addAttribute("isAdmin", isAdmin)
        return "stock"
    }

    @RequestMapping(value = ["/stock/change-price"], method = [RequestMethod.POST])
    fun changePrice(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "stockId", required = true) stockId: Long,
        @RequestParam(name = "price", required = true, defaultValue = 0.toString()) price: Long,
    ): String {
        val stock = stockService.findById(stockId) ?: error("Stock not found")
        stock.price = price
        stockService.save(stock)
        return "redirect:/stock?userId=${userId}&id=${stockId}"
    }

    @RequestMapping(value = ["/stock/delete-stock"], method = [RequestMethod.POST])
    fun deleteStock(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        val stock = stockService.findById(id) ?: error("Task wasn't found")
        stockService.remove(id)
        return "redirect:/company?userId=${userId}&id=${stock.companyId}"
    }
}
