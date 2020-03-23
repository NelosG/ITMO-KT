package queue;

public abstract class AbstractQueue implements Queue {
    public int size = 0;

    @Override
    public int size() {
        return size;
    }


    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public Object dequeue() {
        assert size > 0;
        Object res =  element();
        deleteFirst();
        return res;
    }

    @Override
    public Object[] toArray() {
        Object[] res = new Object[size];
        for (int i = 0; i < size; i++) {
            res[i] = dequeue();
            enqueue(res[i]);
        }
        return res;
    }

    protected void deleteFirst() {}
}
