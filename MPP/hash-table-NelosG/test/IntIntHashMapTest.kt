import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.LoggingLevel.*
import org.jetbrains.kotlinx.lincheck.annotations.*
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.paramgen.*
import org.jetbrains.kotlinx.lincheck.strategy.stress.*
import org.jetbrains.kotlinx.lincheck.verifier.*
import org.junit.*

@Param.Params(
    Param(name = "key", gen = IntGen::class, conf = "1:8"),
    Param(name = "value", gen = IntGen::class, conf = "1:10")
)
class IntIntHashMapTest {
    private val map = IntIntHashMap()

    @Operation
    fun put(@Param(name = "key") key: Int, @Param(name = "value") value: Int): Int = map.put(key, value)

    @Operation
    fun remove(@Param(name = "key") key: Int): Int = map.remove(key)

    @Operation
    fun get(@Param(name = "key") key: Int): Int = map.get(key)

    @Test
    fun runTest() = StressOptions()
        .iterations(100)
        .invocationsPerIteration(50_000)
        .actorsBefore(2)
        .threads(3)
        .actorsPerThread(5)
        .sequentialSpecification(IntIntHashMapSequential::class.java)
        .logLevel(INFO)
        .check(this::class.java)
}

class IntIntHashMapSequential : VerifierState() {
    private val map = HashMap<Int, Int>()

    fun put(key: Int, value: Int): Int = map.put(key, value) ?: 0
    fun remove(key: Int): Int = map.remove(key) ?: 0
    fun get(key: Int): Int = map.get(key) ?: 0

    override fun extractState() = map
}