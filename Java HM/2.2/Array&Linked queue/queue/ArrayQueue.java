package queue;

import java.util.Arrays;

public class ArrayQueue  extends AbstractQueue{
    private Object[] elements = new Object[16];
    private int head = 0;
    private int tail = 0;

    public void enqueue(Object element) {
        ensureCapacity();   
        elements[tail] = element;
        tail = (tail + 1) % elements.length;
        size++;
    }

    public Object element() {
        assert size > 0;
        return elements[head];
    }

    @Override
    protected void deleteFirst() {
        head = (head + 1) % elements.length;
        size--;
    }

//    public Object[] toArray() {
//        Object result[] = new Object[size()];
//        for (int i = 0; i < result.length; i++) {
//            result[i] = elements[(head + i) % elements.length];
//        }
//        return result;
//    }

    public void clear() {
        elements = new Object[16];
        head = 0;
        tail = 0;
        size = 0;
    }

    private void ensureCapacity() {
        if (size() == elements.length - 1) {
            Object tmp[] = toArray();
            elements = Arrays.copyOf(tmp, tmp.length * 2);
            head = 0;
            tail = tmp.length;
        }
    }
}