package ru.ifmo.pga.software.design.news.module.stats.parser.exception

class VkResponseParserException : RuntimeException {
    constructor() : super()

    constructor(message: String) : super(message)

    constructor(message: String, cause: Throwable) : super(message, cause)

    constructor(cause: Throwable) : super(cause)
}