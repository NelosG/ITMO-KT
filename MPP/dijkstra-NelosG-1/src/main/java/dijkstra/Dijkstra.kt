package dijkstra

import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.locks.ReentrantLock
import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.ThreadLocalRandom
import kotlin.Comparator
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> o1!!.distance.compareTo(o2!!.distance) }
private val active = atomic(1)
// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = MultiQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    active.lazySet(1)
    repeat(workers) {
        thread {
            while (active.value > 0) {
                val cur: Node = q.poll() ?: continue
                for (e in cur.outgoingEdges) {
                    while (true) {
                        val distance = e.to.distance
                        val newDistance = cur.distance + e.weight
                        if (distance > newDistance) {
                            if (e.to.casDistance(distance, newDistance)) {
                                q.add(e.to)
                                active.incrementAndGet()
                            } else continue
                        }
                        break
                    }
                }
                active.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

class MultiQueue<T : Any>(private val n: Int, private var comparator: Comparator<T>) {
    private val queues: MutableList<PriorityQueue<T>> = Collections.nCopies(n, PriorityQueue(n, comparator))
    private val locks: MutableList<ReentrantLock> = Collections.nCopies(n, ReentrantLock()) // faster then synchronized

    fun poll(): T? {
        val random = ThreadLocalRandom.current()
        val i = random.nextInt(n)
        var j = random.nextInt(n)
        while (i == j) {
            j = random.nextInt(n)
        }
        locks[i].lock()
        locks[j].lock()
        val queueIPeek = queues[i].peek()
        val queueJPeek = queues[j].peek()
        return try {
            when {
                queueIPeek == null && queueJPeek == null -> null
                queueIPeek == null -> queues[j].poll()
                queueJPeek == null -> queues[i].poll()
                else -> if (comparator.compare(queueIPeek, queueJPeek) < 0) queues[i].poll() else queues[j].poll()
            }
        } finally {
            locks[j].unlock()
            locks[i].unlock()

        }
    }


    fun add(element: T) {
        val index = ThreadLocalRandom.current().nextInt(n)
        try {
            locks[index].lock()
            queues[index].add(element)
        } finally {
            locks[index].unlock()
        }
    }
}
