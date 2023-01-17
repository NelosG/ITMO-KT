package ru.ifmo.pga.software.design.aop.aspect

import guru.nidi.graphviz.model.Factory
import guru.nidi.graphviz.model.MutableNode
import org.springframework.stereotype.Component
import java.util.concurrent.ConcurrentHashMap

@Component
class Euler {
    val parents = ConcurrentHashMap<String, Vertex?>()
    fun into(login: String, mark: String?) {
        if (!parents.containsKey(login)) {
            parents[login] = Vertex.createVertex(null, login)
        }
        val vertex = parents[login]
        val newVertex = Vertex.createVertex(vertex, mark)
        vertex!!.add(newVertex)
        parents[login] = newVertex
    }

    fun onto(login: String, time: Long) {
        val vertex = parents[login]
        vertex!!.time = time
        parents[login] = vertex.parent!!
    }

    class Vertex private constructor(val parent: Vertex?, private val mark: String?) {
        private val childs: ArrayList<Vertex> = ArrayList()
        var time: Long = 0

        val node: MutableNode
            get() {
                val node = Factory.mutNode((mark ?: "null") + " " + time / 1e6 + " ms time")
                for (vertex in childs) {
                    node.addLink(vertex.node)
                }
                return node
            }

        fun add(child: Vertex) {
            childs.add(child)
        }

        companion object {
            fun createVertex(parent: Vertex?, mark: String?): Vertex {
                return Vertex(parent, mark)
            }
        }
    }
}
