package ru.ifmo.pga

class DoubleLinkedList<T> : Iterable<Node<T>> {
    private var head: Node<T>? = null
    private var tail: Node<T>? = null
    private var size = 0

    /**
     * - Creates node with given value
     * - Adds new node in list
     * - Places this node as head
     * 
     * @return created node
     */
    fun add(value: T): Node<T> {
        return checkInvariant {
            val node = Node(value)
            if (head == null) {
                assert(tail == null)
                head = node
                tail = node
            } else {
                node.next = head
                head!!.prev = node
                head = node
            }
            size++
            node
        }.also {
            assert(it.value == value)
            assert(it in this)
        }
    }

    /**
     * Removes tail node in list
     * @return removed value
     */
    fun remove(): T {
        assert(tail != null)
        return checkInvariant {
            val value = tail!!.value
            if (tail!!.prev == null) {
                head = null
                tail = null
            } else {
                tail = tail!!.prev
                tail!!.next = null
            }
            size--
            value
        }
    }

    /**
     * Removes given node in list
     */
    fun remove(node: Node<T>) {
        assert(node in this)
        checkInvariant {
            if (node.prev == null) {
                assert(node == head)
                head = node.next
            } else {
                node.prev!!.next = node.next
            }
            
            if (node.next == null) {
                assert(node == tail)
                tail = node.prev
            } else {
                node.next!!.prev = node.prev
            }
            size--
        }
        assert(node !in this)
    }

    fun size(): Int = size

    fun first(): Node<T>? = head

    fun last(): Node<T>? = tail

    override fun iterator(): Iterator<Node<T>> = object : Iterator<Node<T>> {
        var current = head

        override fun hasNext(): Boolean = current != null

        override fun next(): Node<T> {
            assert(current != null)
            val node = current!!
            current = node.next
            return node
        }
    }

    private fun assertionCheck() {
        assert(size >= 0)
        if (size == 0) {
            assert(head == null)
            assert(tail == null)
            return
        }
        assert(head != null)
        assert(tail != null)
        assert(head?.prev == null)
        assert(tail?.next == null)
        if (size == 1) {
            assert(head == tail)
        }
        assert(iterator().asSequence().count() == size)
    }

    private inline fun <R> checkInvariant(fn: DoubleLinkedList<T>.() -> R): R {
        return fn().also { assertionCheck() }
    }
}