package ru.ifmo.pga.software.design.exchange.web

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import ru.ifmo.pga.software.design.exchange.service.StockService
import ru.ifmo.pga.software.design.exchange.service.StockToWalletService
import ru.ifmo.pga.software.design.exchange.service.UserService
import ru.ifmo.pga.software.design.exchange.service.WalletService
import ru.ifmo.pga.software.design.exchange.web.vo.StockVo

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Controller
class UserController @Autowired constructor(
    private val userService: UserService,
    private val stockToWalletService: StockToWalletService,
    private val stockService: StockService,
    private val walletService: WalletService,
) {

    @RequestMapping(value = ["/user"], method = [RequestMethod.GET])
    fun getStartPage(
        @RequestParam(name = "id", required = true) id: Long,
        model: Model
    ): String {
        val user = userService.findById(id) ?: error("User not found")
        val stocksToWallet = stockToWalletService.findByUserId(user.id!!)
            .associateBy { it.stockId!! }
        val stocks = stocksToWallet.keys.map { stockService.findById(it)!! }

        val wallet = walletService.findByUserId(id) ?: error("wallet not found")

        val stockVos = stocks.map { stock ->
            StockVo(
                id = stock.id!!,
                name = stock.name!!,
                price = stock.price!!,
                count = stocksToWallet[stock.id!!]!!.count!!
            )
        }
        var allMoney = wallet.amount!!
        stocks.forEach{ stock ->
            allMoney += stock.price!! * stocksToWallet[stock.id!!]!!.count!!
        }
        model.addAttribute("user", user)
        model.addAttribute("wallet", wallet)
        model.addAttribute("stocks", stockVos)
        model.addAttribute("allMoney", allMoney)
        return "user"
    }

    @RequestMapping(value = ["/user/buy-stock"], method = [RequestMethod.POST])
    fun buyStock(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "stockId", required = true) stockId: Long,
        @RequestParam(name = "count", required = true) count: Long,
        model: Model
    ): String {
        stockService.buyStock(userId, stockId, count)
        return "redirect:/stock?userId=${userId}&id=${stockId}"
    }

    @RequestMapping(value = ["/user/sell-stock"], method = [RequestMethod.POST])
    fun sellStock(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "stockId", required = true) stockId: Long,
        @RequestParam(name = "count", required = true) count: Long,
        model: Model
    ): String {
        stockService.sellStock(userId, stockId, count)
        return "redirect:/user?id=${userId}"
    }
}