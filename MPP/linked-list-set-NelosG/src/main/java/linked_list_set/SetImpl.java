package linked_list_set;

import java.util.concurrent.atomic.AtomicMarkableReference;

public class SetImpl implements Set {
    private static class Node {
        final AtomicMarkableReference<Node> next;
        int x;

        Node(int x, Node next, boolean toMark) {
            this.next = new AtomicMarkableReference<>(next, toMark);
            this.x = x;
        }
    }

    private static class Window {
        Node cur, next;
    }

    private final Node head = new Node(Integer.MIN_VALUE, new Node(Integer.MAX_VALUE, null, false), false);

    /**
     * Returns the {@link Window}, where cur.x < x <= next.x
     */
    private Window findWindow(int x) {
        boolean[] removed = new boolean[1];
        while (true) {
            Node cur = head;
            Node next = cur.next.getReference();

            boolean flag = false;
            while (next.x < x) {
                Node node = next.next.get(removed);
                if (removed[0]) {
                    if (cur.next.compareAndSet(next, node, false, false)) {
                        next = node;
                    } else {
                        flag = true;
                        break;
                    }
                } else {
                    cur = next;
                    next = cur.next.getReference();
                }
            }
            if(flag) {
                continue;
            }

            Node nextNext = next.next.get(removed);
            if (removed[0]) {
                cur.next.compareAndSet(next, nextNext, false, false);
            } else {
                Window window = new Window();
                window.cur = cur;
                window.next = next;
                return window;
            }
        }
    }

    @Override
    public boolean add(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.next.x == x) {
                return false;
            }
            Node node = new Node(x, w.next, false);
            if (w.cur.next.compareAndSet(w.next, node, false, false)) {
                return true;
            }
        }
    }

    @Override
    public boolean remove(int x) {
        while (true) {
            Window w = findWindow(x);
            if (w.next.x != x) {
                return false;
            } else {
                Node nextNext = w.next.next.getReference();
                if (w.next.next.compareAndSet(nextNext, nextNext, false, true)) {
                    w.cur.next.compareAndSet(w.next, nextNext, false, false);
                    return true;
                }
            }
        }
    }

    @Override
    public boolean contains(int x) {
        Window w = findWindow(x);
        return w.next.x == x;
    }
}
