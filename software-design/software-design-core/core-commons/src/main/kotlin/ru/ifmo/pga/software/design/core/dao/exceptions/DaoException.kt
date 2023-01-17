package ru.ifmo.pga.software.design.core.dao.exceptions

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
class DaoException : RuntimeException {
    constructor() : super()
    constructor(message: String?) : super(message)
    constructor(message: String?, cause: Throwable?) : super(message, cause)
    constructor(cause: Throwable?) : super(cause)
}
