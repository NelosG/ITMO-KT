package msqueue;

import kotlinx.atomicfu.AtomicRef;

public class MSQueue implements Queue {
    private final AtomicRef<Node> Head;
    private final AtomicRef<Node> Tail;

    public MSQueue() {
        final Node dummy = new Node(0);
        this.Head = new AtomicRef<>(dummy);
        this.Tail = new AtomicRef<>(dummy);
    }

    @Override
    public void enqueue(final int x) {
        final Node node = new Node(x);
        Node tail;
        while (true) {
            tail = this.Tail.getValue();
            if (tail.next.getValue() == null) {
                if (tail.next.compareAndSet(null, node)) {
                    break;
                }
            } else {
                this.Tail.compareAndSet(tail, tail.next.getValue());
            }
        }
        this.Tail.compareAndSet(tail, node);
    }

    @Override
    public int dequeue() {
        while (true) {
            final Node head = this.Head.getValue();
            final Node tail = this.Tail.getValue();
            final Node next = head.next.getValue();
            if (head == this.Head.getValue()) {
                if (head == tail) {
                    if (next == null) {
                        return Integer.MIN_VALUE;
                    }
                    this.Tail.compareAndSet(tail, next);
                } else {
                    if (this.Head.compareAndSet(head, next)) {
                        return next.x;
                    }
                }
            }
        }
    }

    @Override
    public int peek() {
        final Node next = this.Head.getValue().next.getValue();
        if (next == null) {
            return Integer.MIN_VALUE;
        }
        return next.x;
    }

    private static class Node {
        final int x;
        final AtomicRef<Node> next;

        Node(final int x) {
            this.x = x;
            this.next = new AtomicRef<>(null);
        }
    }
}