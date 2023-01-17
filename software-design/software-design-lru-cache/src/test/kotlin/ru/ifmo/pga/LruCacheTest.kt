package ru.ifmo.pga

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test


class LruCacheTest {
    
    @Test
    fun emptyOnCreation() {
        val cache = LruCache<Int, Int>(8)
        
        assertTrue(cache.get(1) == null)
    }

    @Test
    fun getFromEmpty() {
        val cache = LruCache<Int, Int>(1)
        
        assertTrue(cache.get(1) == null)
    }

    @Test
    fun oneEntry() {
        val cache = LruCache<Int, Int>(1)
        cache.put(1, 11)
        
        assertTrue(cache.get(1) == 11)
        assertTrue(cache.get(11) == null)
    }

    @Test
    fun cacheAutoRemoveLeastRecentlyPutItem() {
        val cache = LruCache<Int, Int>(2)
        cache.put(1, 11)
        cache.put(2, 22)
        cache.put(3, 33)
        
        assertTrue(cache.get(1) == null)
        assertTrue(cache.get(2) == 22)
        assertTrue(cache.get(3) == 33)
    }

    @Test
    fun cacheAutoRemoveLeastRecentlyObtainedItem() {
        val cache = LruCache<Int, Int>(2)
        cache.put(1, 11)
        cache.put(2, 22)
        cache.get(1)
        cache.put(3, 33)
        
        assertTrue(cache.get(1) == 11)
        assertTrue(cache.get(2) == null)
        assertTrue(cache.get(3) == 33)
    }

    @Test
    fun overridesValueIfPresent() {
        val cache = LruCache<Int, Int>(1)
        cache.put(1, 11)
        cache.put(1, 22)
        
        assertTrue(cache.get(1) == 22)
    }

    @Test
    fun failOnIncorrectCapacity() {
        assertThrows(
            AssertionError::class.java,
            {
                LruCache<Int, Int>(0)
            },
            "Expected AssertionError"
        )
        assertThrows(
            AssertionError::class.java,
            {
                LruCache<Int, Int>(-1)
            },
            "Expected AssertionError"
        )
    }
}