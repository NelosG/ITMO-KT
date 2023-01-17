package ru.ifmo.pga.software.design.aop.aspect

import org.springframework.stereotype.Component
import java.util.concurrent.ConcurrentHashMap

@Component
class Timer {
    val map: MutableMap<String, MutableList<Long>> = ConcurrentHashMap()
    fun add(clazz: String, time: Long) {
        if (!map.containsKey(clazz)) {
            map[clazz] = ArrayList()
        }
        map[clazz]!!.add(time)
    }
}
