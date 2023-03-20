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
class WalletController @Autowired constructor(
    private val walletService: WalletService,
) {

    @RequestMapping(value = ["/wallet/add-money"], method = [RequestMethod.POST])
    fun addMoney(
        @RequestParam(name = "userId", required = true) userId: Long,
        @RequestParam(name = "walletId", required = true) walletId: Long,
        @RequestParam(name = "amount", required = true) amount: Long,
        model: Model
    ): String {
        walletService.addMoney(walletId, amount)
        return "redirect:/user?id=${userId}"
    }
}