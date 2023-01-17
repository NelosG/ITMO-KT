package ru.ifmo.pga.software.design.aop

import guru.nidi.graphviz.engine.Format
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.model.Factory
import guru.nidi.graphviz.model.Graph
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.CommandLineRunner
import org.springframework.context.ApplicationContext
import org.springframework.stereotype.Component
import ru.ifmo.pga.software.design.aop.aspect.Euler
import ru.ifmo.pga.software.design.aop.aspect.Timer
import ru.ifmo.pga.software.design.aop.entity.Task
import ru.ifmo.pga.software.design.aop.service.TaskService
import java.io.File
import java.nio.file.Files

/**
 * @author Gleb Pushkarev
 * @since 1.0.0
 */
@Component
open class Runner : CommandLineRunner {

    @Autowired
    lateinit var euler: Euler

    @Autowired
    lateinit var timer: Timer

    private val logger: Logger = LoggerFactory.getLogger(javaClass)

    @Autowired
    lateinit var ctx: ApplicationContext


    override fun run(vararg args: String?) {
        try {
            val taskService = ctx.getBean(TaskService::class.java)
            val list = mutableListOf<Task>()
            repeat(5) {
                list.add(Task().apply {
                    name = it.toString()
                })
            }
            taskService.save(list)
            taskService.remove(taskService.findAll())
            stop()
        } catch (e: Exception) {
            logger.error(e.message)
            e.printStackTrace()
        }
    }

    private fun stop() {
        if (timer.map.entries.isNotEmpty()) {
            val file = File("result/result.txt")
            file.parentFile.mkdir()
            Files.newBufferedWriter(file.toPath()).use { writer ->
                for ((key, value) in timer.map.entries) {
                    writer.write("=== $key ===")
                    writer.newLine()
                    var total = 0.0
                    for (e in value) {
                        val ms = e / 1e6
                        total += ms
                        writer.write("$ms ms ")
                        writer.newLine()
                    }
                    writer.write("Average: ${total / value.size} ms ")
                    writer.newLine()
                    writer.write("Total executions: ${value.size} ")
                    writer.newLine()
                }
                writer.flush()
            }
        }
        for ((key, value) in euler.parents.entries) {
            val g: Graph = Factory.mutGraph(key).add(value?.node ?: error("Null node")).toImmutable()
            Graphviz.fromGraph(g).width(WIDTH).render(Format.PNG).toFile(
                File(
                    "result/$key.png"
                )
            )
        }
    }

    companion object {
        private const val WIDTH = 10000
    }
}
