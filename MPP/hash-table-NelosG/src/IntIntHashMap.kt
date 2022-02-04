import kotlinx.atomicfu.AtomicIntArray
import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import java.lang.IllegalStateException

/**
 * Int-to-Int hash map with open addressing and linear probes.
 *
 *
 */
class IntIntHashMap {
    private val core = atomic(Core(INITIAL_CAPACITY))

    /**
     * Returns value for the corresponding key or zero if this key is not present.
     *
     * @param key a positive key.
     * @return value for the corresponding or zero if this key is not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    operator fun get(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        var res = core.value.getInternal(key)
        while (res == WAIT) {
            res = core.value.getInternal(key)
        }
        return toValue(res)
    }

    /**
     * Changes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key   a positive key.
     * @param value a positive value.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key or value are not positive, or value is equal to
     * [Integer.MAX_VALUE] which is reserved.
     */
    fun put(key: Int, value: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        require(isValue(value)) { "Invalid value: $value" }
        return toValue(putAndRehashWhileNeeded(key, value))
    }

    /**
     * Removes value for the corresponding key and returns old value or zero if key was not present.
     *
     * @param key a positive key.
     * @return old value or zero if this key was not present.
     * @throws IllegalArgumentException if key is not positive.
     */
    fun remove(key: Int): Int {
        require(key > 0) { "Key must be positive: $key" }
        return toValue(putAndRehashWhileNeeded(key, DEL_VALUE))
    }

    private fun putAndRehashWhileNeeded(key: Int, value: Int): Int {
        while (true) {
            val actualCore = core.value
            when (val oldValue = actualCore.putInternal(key, value)) {
                NEEDS_REHASH -> core.compareAndSet(actualCore, actualCore.rehash())
                else -> return oldValue
            }
        }
    }

    private class Core internal constructor(val capacity: Int) {
        // Pairs of <key, value> here, the actual
        // size of the map is twice as big.
        val map = AtomicIntArray(capacity * 2)
        val shift: Int
        val next: AtomicRef<Core?> = atomic(null)

        init {
            val mask = capacity - 1
            assert(mask > 0 && mask and capacity == 0) { "Capacity must be power of 2: $capacity" }
            shift = 32 - Integer.bitCount(mask)
        }

        fun getInternal(key: Int): Int {
            var index = index(key)
            var probes = 0
            while (true) {
                val curVal = map[index + 1].value
                when (map[index].value) {
                    NULL_KEY -> return if (curVal == MOVED) WAIT else NULL_VALUE
                    key -> return when (curVal) {
                        MOVED -> WAIT
                        else -> {
                            if(isMoved(curVal)){
                                clearMoved(curVal)
                            } else {
                                curVal
                            }
                        }
                    }
                    else -> {
                        if (++probes >= MAX_PROBES) return NULL_VALUE
                        index = (index + 2) % (capacity * 2)
                    }
                }
            }
        }

        fun putInternal(key: Int, value: Int): Int {
            var index = index(key)
            var probes = 0
            while (true) {
                val curVal = map[index + 1].value
                when (map[index].value) {
                    NULL_KEY -> {
                        if (curVal == DEL_VALUE) return NULL_VALUE
                        if (isMoved(curVal)) return NEEDS_REHASH
                        if (map[index].compareAndSet(NULL_KEY, key)
                            && map[index + 1].compareAndSet(curVal, value)
                        ) return curVal
                    }
                    key -> {
                        if (isMoved(curVal)) return NEEDS_REHASH
                        if (map[index + 1].compareAndSet(curVal, value)) return curVal
                    }
                    else -> {
                        if (++probes >= MAX_PROBES) return NEEDS_REHASH
                        index = (index + 2) % (capacity * 2)
                    }
                }
            }
        }

        fun rehash(): Core {
            next.compareAndSet(null, Core(capacity * 2))
            val nextCore = next.value!!
            var index = 0
            while (true) {
                val curVal = map[index + 1].value
                val curKey = map[index].value
                if (curVal != MOVED) {
                    if (isMoved(curVal)) {
                        val cleared = clearMoved(curVal)
                        if (isValue(cleared)) {
                            var probes = 0
                            var ind = nextCore.index(curKey)
                            while (true) {
                                when (val newCurKey = nextCore.map[ind].value) {
                                    NULL_KEY -> if (nextCore.map[ind].compareAndSet(newCurKey, curKey)) {
                                        nextCore.map[ind + 1].compareAndSet(NULL_VALUE, cleared)
                                        break
                                    }
                                    curKey -> {
                                        nextCore.map[ind + 1].compareAndSet(NULL_VALUE, cleared)
                                        break
                                    }
                                    else -> {
                                        if (++probes >= MAX_PROBES) {
                                            throw IllegalStateException("Unable to extend hashtable")
                                        }
                                        ind = (ind + 2) % (nextCore.capacity * 2)
                                    }
                                }
                            }
                            map[index + 1].value = MOVED
                        }
                    } else {
                        map[index + 1].compareAndSet(curVal, setMoved(curVal))
                        continue
                    }
                }
                index += 2
                if(index >= capacity * 2) {
                    break
                }
            }
            return nextCore
        }

        /**
         * Returns an initial index in map to look for a given key.
         */
        fun index(key: Int): Int = (key * MAGIC ushr shift) * 2
    }
}

private const val MAGIC = -0x61c88647 // golden ratio
private const val INITIAL_CAPACITY = 2 // !!! DO NOT CHANGE INITIAL CAPACITY !!!
private const val MAX_PROBES = 8 // max number of probes to find an item
private const val NULL_KEY = 0 // missing key (initial value)
private const val NULL_VALUE = 0 // missing value (initial value)
private const val DEL_VALUE = Int.MAX_VALUE // mark for removed value
private const val NEEDS_REHASH = -1 // returned by `putInternal` to indicate that rehash is needed
private const val WAIT = -2
private const val MOVED = Int.MIN_VALUE

// Checks is the value is in the range of allowed values
private fun isValue(value: Int): Boolean = value in (1 until DEL_VALUE)

// Converts internal value to the public results of the methods
private fun toValue(value: Int): Int = if (isValue(value)) value else 0

private fun isMoved(value: Int): Boolean = (value and MOVED) != 0

private fun setMoved(value: Int): Int = value or MOVED

private fun clearMoved(value: Int): Int = value and MOVED.inv()
