import kotlinx.atomicfu.*

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    private val sz = atomic(0)

    private fun check(index: Int) {
        if (index >= size) {
            throw IllegalArgumentException("Index ($index) is illegal")
        }
    }

    override fun get(index: Int): E {
        check(index)
        return core.value.array[index].value!!.value
    }

    override fun put(index: Int, element: E) {
        check(index)
        var curCore = core.value

        while (true) {
            when (val cur = curCore.array[index].value!!) {
                is Basic -> {
                    if (curCore.array[index].compareAndSet(cur, Basic(element))) {
                        return
                    }
                }
                is Fixed, is Moved -> {
                    move(index)
                    val curNext = curCore.next.value!!
                    core.compareAndSet(curCore, curNext)
                    curCore = core.value
                }
            }
        }
    }

    override fun pushBack(element: E) {
        var curCore = core.value

        while (true) {
            val curSize = size
            val curCapacity = curCore.capacity

            if (curSize < curCapacity) {
                if (curCore.array[curSize].compareAndSet(null, Basic(element))) {
                    sz.incrementAndGet()
                    return
                }
            } else {
                if(curCore.next.compareAndSet(null, Core(2 * curCapacity))) {
                    move(0)
                    core.compareAndSet(curCore, curCore.next.value!!)
                }
                curCore = core.value
            }
        }
    }

    override val size: Int
        get() {
            return sz.value
        }

    private fun move(index: Int) {
        val curCore = core.value
        val nextCore = curCore.next.value ?: return

        var ind = index
        while (ind < curCore.capacity) {
            when (val current = curCore.array[ind].value!!) {
                is Moved -> ++ind
                is Basic -> {
                    val fixedVal = Fixed(current)
                    if (curCore.array[ind].compareAndSet(current, fixedVal)) {
                        nextCore.array[ind].compareAndSet(null, current)
                        curCore.array[ind++].compareAndSet(fixedVal, Moved(current))
                    }
                }
                is Fixed -> {
                    nextCore.array[ind].compareAndSet(null, Basic(current))
                    curCore.array[ind++].compareAndSet(current, Moved(current))
                }
            }
        }
    }
}

private class Core<E>(val capacity: Int) {
    val array = atomicArrayOfNulls<Wrapper<E>>(capacity)
    val next = atomic<Core<E>?>(null)
}

private open class Wrapper<E>(val value: E)

private class Fixed<E>(e: Wrapper<E>) : Wrapper<E>(e.value)

private class Moved<E>(e: Wrapper<E>) : Wrapper<E>(e.value)

private class Basic<E>(e: Wrapper<E>) : Wrapper<E>(e.value) {
    constructor(value: E) : this(Wrapper(value))
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME
