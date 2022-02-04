import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

class SynchronousQueueMS<E> : SynchronousQueue<E> {
    private val head: AtomicRef<Node<E>>
    private val tail: AtomicRef<Node<E>>

    init {
        val dummy = Node<E>(null, null)
        head = atomic(dummy)
        tail = atomic(dummy)
    }

    override suspend fun send(element: E) {
        while (true) {
            val curHead = head.value
            val curTail = tail.value
            if (curHead == curTail || curTail.operation.value == Operation.SEND) {
                if (!corut(curTail, Node(Operation.SEND, element))) {
                    return
                }
            } else {
                val curNext = curHead.next.value ?: continue
                if (curNext.cor.value != null && head.compareAndSet(curHead, curNext)) {
                    val curVal = curNext.element.value
                    curNext.element.compareAndSet(curVal, element)
                    curNext.cor.value!!.resume(false)
                    return
                }
            }
        }
    }

    override suspend fun receive(): E {
        while (true) {
            val curHead = head.value
            val curTail = tail.value
            if (curHead == curTail || curTail.operation.value == Operation.RECEIVE) {
                val node = Node<E>(Operation.RECEIVE, null)
                if (!corut(curTail, node)) {
                    return node.element.value!!
                }
            } else {
                val curNext = curHead.next.value ?: continue
                if (curNext.cor.value != null && head.compareAndSet(curHead, curNext)) {
                    curNext.cor.value!!.resume(false)
                    return curNext.element.value!!
                }
            }
        }
    }
    private suspend fun corut(curTail: Node<E>, node: Node<E>): Boolean {
        return suspendCoroutine<Boolean> sc@{ cor ->
            node.cor.value = cor
            if (curTail.next.compareAndSet(null, node)) {
                tail.compareAndSet(curTail, node)
            } else {
                tail.compareAndSet(curTail, curTail.next.value!!)
                cor.resume(true)
                return@sc
            }
        }
    }
}

enum class Operation {
    SEND,
    RECEIVE
}

private class Node<E>(type: Operation?, elem: E?) {
    val operation: AtomicRef<Operation?> = atomic(type)
    val element: AtomicRef<E?> = atomic(elem)
    val next: AtomicRef<Node<E>?> = atomic(null)
    val cor: AtomicRef<Continuation<Boolean>?> = atomic(null)
}
