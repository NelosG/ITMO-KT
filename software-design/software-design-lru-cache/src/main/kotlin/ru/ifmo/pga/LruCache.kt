package ru.ifmo.pga

class LruCache<K, V>(private val capacity: Int) {
    private val data: MutableMap<K, Node<Pair<K, V>>>
    private val linkedList: DoubleLinkedList<Pair<K, V>> = DoubleLinkedList()

    init {
        assert(capacity > 0)
        data = HashMap(capacity)
    }

    fun get(key: K): V? {
        return keepInvariant {
            data[key]?.let { node ->
                linkedList.remove(node)
                data[key] = linkedList.add(node.value)
                assert(linkedList.first()?.value?.first == key)
                node.value.second
            }
        }
    }

    fun put(key: K, value: V) {
        return keepInvariant {
            data[key]?.let { node ->
                linkedList.remove(node)
            } ?: run {
                if (linkedList.size() == capacity) {
                    val last = linkedList.remove()
                    data.remove(last.first)
                }
            }
            data[key] = linkedList.add(key to value)
            assert(linkedList.first()?.value?.first == key)
            assert(linkedList.first()?.value?.second == value)
        }
    }

    private fun assertionCheck() {
        assert(data.size <= capacity)
        assert(data.size == linkedList.size())
    }

    private inline fun <R> keepInvariant(fn: LruCache<K, V>.() -> R): R {
        return fn().also { assertionCheck() }
    }
}