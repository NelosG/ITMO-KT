package stack;

import kotlinx.atomicfu.AtomicArray;
import kotlinx.atomicfu.AtomicRef;

import java.util.concurrent.ThreadLocalRandom;

public class StackImpl implements Stack {

    // head pointer
    private final AtomicRef<Node> H = new AtomicRef<>(null);


    // Variable is read-only (no synchronization is required), static,
    // since it was created at the moment the class is loaded into the classloader,
    // so there is no point in making them volatile
    private final static Slot doneSlot = new Slot(State.DONE, 1);

    private final AtomicArray<Slot> elimination;
    private final ThreadLocalRandom rand = ThreadLocalRandom.current();

    private final int ELIMINATION_SIZE;
    private final int ITERATION_TO_ELIMINATE = 100;

    StackImpl() {
        int count = Runtime.getRuntime().availableProcessors();

        // if we have only 1 processor, the request will uselessly wait for a paired pop request in the array since it will not follow
        if (count == 1 ||

                // At least 1 of all streams can definitely go directly to the sheet
                // also this so that the operating system and other programs are less likely to switch the process running in the kernel
                count > 2) {
            --count;
        }

        this.ELIMINATION_SIZE = count;
        this.elimination = new AtomicArray<>(this.ELIMINATION_SIZE);
    }

    @Override
    public void push(final int x) {
        if (!eliminatePush(x)) {
            // creating a lot of new variables is a bad idea (allocating memory, etc.)
            // so there is a method to change next Node
            final Node newHead = new Node(x, null);
            while (true) {
                final Node head = this.H.getValue();
                if (this.H.compareAndSet(head, newHead.changeNext(head))) {
                    return;
                }
            }
        }
    }

    @Override
    public int pop() {
        final int r = this.rand.nextInt(this.ELIMINATION_SIZE); // random spoils everything
        for (int i = 0, j; i < 5; i++) {
            j = (r + i) % this.ELIMINATION_SIZE;
            final Slot internalSlot = this.elimination.get(j).getValue();
            if (internalSlot != null &&
                    internalSlot.state == State.READY &&
                    this.elimination.get(j).compareAndSet(internalSlot, doneSlot)) {
                return internalSlot.value;
            }
        }

        while (true) {
            final Node head = this.H.getValue();
            if (head == null) {
                return Integer.MIN_VALUE;
            }
            if (this.H.compareAndSet(head, head.next.getValue())) {
                return head.x;
            }
        }
    }

    private boolean eliminatePush(final int x) {
        final int r = this.rand.nextInt(this.ELIMINATION_SIZE);
        final Slot insertSlot = new Slot(State.READY, x);
        for (int i = 0, j; i < 5; i++) {
            j = (r + i) % this.ELIMINATION_SIZE;
            if (this.elimination.get(j).compareAndSet(null, insertSlot)) {
                for(int t = 0; t < this.ITERATION_TO_ELIMINATE; ++t) {
                    if (this.elimination.get(j).compareAndSet(doneSlot, null)) {
                        return true;
                    }
                }

                if (this.elimination.get(j).compareAndSet(insertSlot, null)) {
                    return false;
                } else {
                    if (!this.elimination.get(j).compareAndSet(doneSlot, null)) {
                        throw new IllegalStateException("Someone interfered with the work of the class");
                    }
                    return true;
                }
            }
        }
        return false;
    }

    private enum State {
        READY,
        DONE
    }

    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(final int x, final Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }

        Node changeNext(final Node newNext) {
            this.next.setValue(newNext);
            return this;
        }
    }

    private static class Slot {
        final State state;
        final int value;

        Slot(final State st, final int value) {
            this.state = st;
            this.value = value;
        }
    }
}
