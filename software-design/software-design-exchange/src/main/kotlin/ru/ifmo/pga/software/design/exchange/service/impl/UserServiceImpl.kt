package ru.ifmo.pga.software.design.exchange.service.impl

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Propagation
import org.springframework.transaction.annotation.Transactional
import ru.ifmo.pga.software.design.core.service.impl.ServiceImpl
import ru.ifmo.pga.software.design.exchange.dao.UserDao
import ru.ifmo.pga.software.design.exchange.entity.User
import ru.ifmo.pga.software.design.exchange.entity.Wallet
import ru.ifmo.pga.software.design.exchange.service.UserService
import ru.ifmo.pga.software.design.exchange.service.WalletService

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Service("userService")
open class UserServiceImpl @Autowired constructor(
    private val walletService: WalletService
) : ServiceImpl<User, UserDao>(), UserService {

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entity: User): User {
        val user = super.save(entity)
        walletService.save(Wallet().apply {
            userId = user.id
        })
        return user
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun save(entities: Collection<User>): List<User> {
        val res = super.save(entities)
        res.forEach {
            walletService.save(Wallet().apply {
                userId = it.id
            })
        }
        return res
    }

    override fun isAdmin(id: Long): Boolean {
        return id in ADMIN_USER_IDS
    }

    override fun isAdmin(user: User): Boolean {
        val userId = user.id ?: error("User without id")
        return isAdmin(userId)
    }

    @Transactional(propagation = Propagation.REQUIRED)
    override fun remove(id: Long) {
        walletService.findByUserId(id)?.let { walletService.remove(it) }
    }

    companion object {
        private val ADMIN_USER_IDS = listOf(0L)
    }

}
