import kotlinx.atomicfu.*

class FAAQueue<T> {
    private val head: AtomicRef<Segment> // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private val tail: AtomicRef<Segment> // Tail pointer, similarly to the Michael-Scott queue

    init {
        val firstNode = Segment()
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(x: T) {
        while (true) {
            val tail = tail.value
            val endInd = tail.enqIdx.getAndIncrement()
            if (endInd >= SEGMENT_SIZE) {
                val newSeg = Segment(x)
                val isEnqueued = tail.next.compareAndSet(null, newSeg)
                this.tail.compareAndSet(tail, tail.next.value!!)
                if (isEnqueued) return
            } else {
                if (tail.elements[endInd].compareAndSet(null, x)) {
                    return
                }
            }
        }
    }

    /**
     * Retrieves the first element from the queue
     * and returns it; returns `null` if the queue
     * is empty.
     */
    fun dequeue(): T? {
        while (true) {
            val head = head.value
            val deqInd = head.deqIdx.getAndIncrement()
            if (deqInd >= SEGMENT_SIZE) {
                val headNext = head.next.value ?: return null
                this.head.compareAndSet(head, headNext)
                continue
            }
            val curVal = head.elements[deqInd].getAndSet(DONE) ?: continue
            @Suppress("UNCHECKED_CAST")
            return curVal as T
        }
    }

    /**
     * Returns `true` if this queue is empty;
     * `false` otherwise.
     */
    val isEmpty: Boolean
        get() {
            while (true) {
                val curHead = head.value
                return if (head.value.isEmpty) {
                    val next = curHead.next.value
                    if (next == null) {
                        true
                    } else {
                        head.compareAndSet(curHead, next)
                        continue
                    }
                } else {
                    false
                }
            }
        }
}

private class Segment {
    val next: AtomicRef<Segment?> = atomic(null)
    val enqIdx = atomic(0) // index for the next enqueue operation
    val deqIdx = atomic(0) // index for the next dequeue operation
    val elements = atomicArrayOfNulls<Any>(SEGMENT_SIZE)

    constructor() // for the first segment creation

    constructor(x: Any?) { // each next new segment should be constructed with an element
        enqIdx.compareAndSet(0, 1)
        elements[0].compareAndSet(null, x)
    }

    val isEmpty: Boolean get() = deqIdx.value >= enqIdx.value || deqIdx.value >= SEGMENT_SIZE

}

private val DONE = Any() // Marker for the "DONE" slot state; to avoid memory leaks
const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS
