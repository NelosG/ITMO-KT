package ru.ifmo.pga.software.design.aop.aspect

import org.aspectj.lang.ProceedingJoinPoint
import org.aspectj.lang.annotation.*
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.stereotype.Component


@Aspect
@Component
class AspectLogRunner {

    @Autowired
    lateinit var euler: Euler

    @Autowired
    lateinit var timer: Timer

    @Pointcut("within(ru.ifmo.pga.software.design.aop.dao..*)")
    fun daoMethod() {
    }

    @Pointcut("within(ru.ifmo.pga.software.design.aop.service..*)")
    fun serviceMethod() {
    }

    @Pointcut("@annotation(ru.ifmo.pga.software.design.aop.aspect.AspectProfile)")
    fun annotatedMethod() {
    }

    @Around("daoMethod() || serviceMethod() || annotatedMethod()")
    fun logExecutionTime(joinPoint: ProceedingJoinPoint): Any? {
        val sb = StringBuilder("$joinPoint(")
        for (o in joinPoint.args) {
            val newO = o.javaClass.simpleName
            sb.append(newO)
            sb.append(", ")
        }
        val lastIndex = sb.lastIndexOf(",")
        sb.delete(lastIndex, lastIndex + 2)
        val s = sb.append(")").toString()
        euler.into(joinPoint.toString(), s)
        val start = System.nanoTime()
        val result = joinPoint.proceed(joinPoint.args)
        val exit = System.nanoTime()
        euler.onto(joinPoint.toString(), exit - start)
        timer.add(joinPoint.toString(), exit - start)
        return result
    }
}
