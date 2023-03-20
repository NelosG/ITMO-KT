package ru.ifmo.pga.software.design.reactive.service

import org.springframework.stereotype.Service
import ru.ifmo.pga.software.design.reactive.model.Currency
import java.math.BigDecimal

@Service("currencyService")
class CurrencyService(private val exchangeRateService: ExchangeRateService) {
    fun convert(price: BigDecimal, currencyFrom: Currency, currencyTo: Currency): BigDecimal =
        price / exchangeRateService.exchangeRate(currencyFrom) * exchangeRateService.exchangeRate(currencyTo)
}