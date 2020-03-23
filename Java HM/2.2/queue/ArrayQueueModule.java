package queue;

import java.util.Arrays;

public class ArrayQueueModule {
    // Inv: (n >= 0) && (a[i] != null for i = 1..n - 1)
    private static Object[] elements = new Object[20];
    private static int size = 0;
    private static int head = 0;
    private static int tail = 0;

    // Pre: (elements.length != 0) && (0 <= x < elements.length)
    private static int add(int x) {
        return (x + 1) % elements.length;
    }
    // Result: x' = (x + 1) % elements.length

    // Pre: (elements.length != 0) && (0 <= x < elements.length)
    private static int dec(int x) {
        if (x == 0) {
            return elements.length - 1;
        } else {
            return x - 1;
        }
    }
    // Post: (Result == x - 1 && x > 0) || (Result == elements.length && x == 0)

    //Pre: sz >= 0
    private static void ensureCapacity(int sz) {
        if ((elements.length <= sz) || (sz * 3 < elements.length)) {
            Object[] temp = new Object[sz * 2 + 1];
            int ind = 0;
            for (int i = head; i != tail; i = add(i)) {
                temp[ind++] = elements[i];
            }
            elements = temp;
            head = 0;
            tail = ind;
        }
    }
    // Post: (sz < elem'.length <= sz * 4) && (n' == n) && (a'[i] == a[i] for i = 0...n - 1)

    // Pre: (elem != null)
    public static void enqueue(Object elem) {
        assert elem != null;
        size++;
        ensureCapacity(size);
        elements[tail] = elem;
        tail = add(tail);
    }
    // (n' == n + 1) && (a'[i] == a[i] for i = 0..n - 1) && (a'[n] == elem)

    // Pre: n > 0
    public static Object element() {
        assert size > 0;

        return elements[head];
    }
    // Post: (n' == n) && (a'[i] == a[i] for i = 0...n - 1) && (Result == a[0])

    // Pre: n > 0
    public static Object dequeue() {
        assert size > 0;
        Object ans = elements[head];
        elements[head] = null;
        head = add(head);
        size--;
        ensureCapacity(size);
        return ans;
    }
    // Post: (n' == n - 1) && (a'[i - 1] == a[i] for i = 1...n - 1) && (Result == a[0])

    public static int size() {
        return size;
    }
    // Post: (n' == n) && (a'[i] == a[i] for i = 0...n - 1) && (Result == n)

    // Pre: true
    public static boolean isEmpty() {
        return size == 0;
    }
    // Post: (n' == n) && (a'[i] == a[i] for i = 0...n - 1) && (Result == (n == 0))

    public static void clear() {
        elements = new Object[5];
        size = head = tail = 0;
    }
    // Post: n == 0

//    public static Object[] toArray() {
//        Object result[] = new Object[size()];
//        for (int i = 0; i < result.length; i++) {
//            result[i] = elements[(head + i) % elements.length];
//        }
//        return result;
//    }

    public static Object[] toArray() {
        Object[] res = new Object[size];
        for (int i = 0; i < size; i++) {
            res[i] = element();
            enqueue(dequeue());
        }
        return res;
    }
    // Post: ℝ[n'] && n = n' && ∀ i = 1..n : a[i] = a[i]' && ℝ[i] = a[i]'


    public static String toStr() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        Object temp = null;
        for (int i = 0; i < size - 1; i++) {
            temp = dequeue();
            sb.append(temp).append(", ");
            enqueue(temp);
        }
        if (size != 0) {
            temp = dequeue();
            sb.append(temp);
            enqueue(temp);
        }
        sb.append("]");
        return sb.toString();
    }
    // Post: ℝ = "[" + (∀ i = 1..n : a[i] + ", ") + "]"
    // && n = n' && ∀ i = 1..n : a[i] = a[i]'
}