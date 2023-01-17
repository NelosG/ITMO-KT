package ru.ifmo.pga

data class Node<T>(
    val value: T,
    var next: Node<T>? = null,
    var prev: Node<T>? = null
)