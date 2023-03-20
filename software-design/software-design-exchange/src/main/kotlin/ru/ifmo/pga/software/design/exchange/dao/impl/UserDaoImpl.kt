package ru.ifmo.pga.software.design.exchange.dao.impl

import org.springframework.stereotype.Repository
import ru.ifmo.pga.software.design.core.dao.impl.GenericDaoImpl
import ru.ifmo.pga.software.design.exchange.dao.UserDao
import ru.ifmo.pga.software.design.exchange.entity.User

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Repository("userDao")
open class UserDaoImpl : GenericDaoImpl<User>(), UserDao