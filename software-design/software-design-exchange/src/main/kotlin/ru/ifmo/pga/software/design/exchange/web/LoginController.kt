package ru.ifmo.pga.software.design.exchange.web

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RequestMethod
import org.springframework.web.bind.annotation.RequestParam
import ru.ifmo.pga.software.design.exchange.entity.User
import ru.ifmo.pga.software.design.exchange.service.UserService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Controller
class LoginController @Autowired constructor(
    private val userService: UserService,
) {
    @RequestMapping(value = ["/"], method = [RequestMethod.GET])
    fun getStartPage(model: Model): String {
        return "login"
    }

    @RequestMapping(value = ["/login/sign-in"], method = [RequestMethod.GET])
    fun getStock(
        @RequestParam(name = "userId", required = true) userId: Long,
        model: Model
    ): String {
        userService.findById(userId) ?: error("User not found")
        return "redirect:/user?id=${userId}"
    }

    @RequestMapping(value = ["/login/sign-up"], method = [RequestMethod.POST])
    fun signUp(model: Model): String {
        val user = userService.save(User())
        return "redirect:/user?id=${user.id}"
    }
}