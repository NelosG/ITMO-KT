package ru.ifmo.pga

import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

class DoubleLinkedListTest {
    private lateinit var list: DoubleLinkedList<Int>
    
    @BeforeEach
    fun listInit(){
        list = DoubleLinkedList()
    }

    @Test
    fun emptyList() {
        assert(list.size() == 0)
        assert(list.first() == null)
        assert(list.last() == null)
    }
    
    @Test
    fun oneNode() {
        val node = list.add(1)
        
        assert(list.first() == node)
        assert(list.last() == node)
        assert(list.size() == 1)
    }

    @Test
    fun twoNodes() {
        val node1 = list.add(1)
        val node2 = list.add(2)
        
        assert(list.first() == node2)
        assert(list.last() == node1)
        assert(list.size() == 2)
    }

    @Test
    fun removeOneNode() {
        val node = list.add(1)
        val removedValue = list.remove()
        
        assert(removedValue == 1)
        assert(list.size() == 0)
        assert(node !in list)
    }

    @Test
    fun removeOneOfTwoNodes() {
        val node1 = list.add(1)
        val node2 = list.add(2)
        val removedValue = list.remove()
        
        assert(removedValue == 1)
        assert(list.size() == 1)
        assert(node1 !in list)
        assert(node2 in list)
    }

    @Test
    fun removeTwoNodes() {
        val node1 = list.add(1)
        val node2 = list.add(2)
        list.remove()
        val removedValue = list.remove()
        
        assert(removedValue == 2)
        assert(list.size() == 0)
        assert(node1 !in list)
        assert(node2 !in list)
    }

    @Test
    fun alternatingRemoveAndAddNode() {
        list.add(1)
        val node2 = list.add(2)
        list.remove()
        val node3 = list.add(3)
        
        assert(list.size() == 2)
        assert(node2 in list)
        assert(node3 in list)
        
        list.remove()
        
        assert(list.size() == 1)
        assert(node2 !in list)
        assert(node3 in list)
    }
}