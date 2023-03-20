package ru.ifmo.pga.software.design.reactive.service

import org.springframework.stereotype.Service
import ru.ifmo.pga.software.design.reactive.model.Currency
import java.math.BigDecimal

@Service("exchangeRateService")
class ExchangeRateService {
    fun exchangeRate(currency: Currency): BigDecimal =
        when (currency) {
            Currency.USD -> BigDecimal.ONE
            Currency.EUR -> BigDecimal.valueOf(1.08)
            Currency.RUB -> BigDecimal.valueOf(0.013)
        }
}