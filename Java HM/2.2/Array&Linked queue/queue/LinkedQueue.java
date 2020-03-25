package queue;

public class LinkedQueue extends AbstractQueue {
    class Nu {
    }

    class Node {
        Node next = null;
        Object element = new Nu();
    }
    private Node head = new Node();
    private Node curLast = head;

    @Override
    public void enqueue(Object element) {
        if (curLast.element.getClass() != Nu.class) {
            curLast.next = new Node();
            curLast = curLast.next;
        }
        curLast.element = element;
        size++;
    }

    @Override
    public Object element() {
        assert size > 0;
        return head.element;
    }

    @Override
    protected void deleteFirst() {
        if (head.next != null) {
            head = head.next;
        } else {
            head.element = new Nu();
        }
        size--;
    }

    @Override
    public void clear() {
        head = new Node();
        curLast = head;
        size = 0;
    }

//    @Override
//    public Object[] toArray() {
//        Object[] res = new Object[size];
//        Node temp = head;
//        for (int i = 0; i < size; i++) {
//            res[i] = temp.element;
//            temp = temp.next;
//        }
//        return res;
//    }
}
