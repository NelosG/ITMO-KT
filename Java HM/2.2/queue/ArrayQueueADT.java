package queue;

import java.util.Arrays;

public class ArrayQueueADT {
    // Inv: (n >= 0) && (a[i] != null for i = 1..n - 1)
    private Object[] elements = new Object[20];
    private static int size = 0;
    private static int head = 0;
    private static int tail = 0;

    // Pre: (elements.length != 0) && (0 <= x < elements.length) && (que != null)
    private static int dec(ArrayQueueADT que, int x) {
        if (x == 0) {
            return que.elements.length - 1;
        } else {
            return x - 1;
        }
    }
    // Result: x' = (x + 1) % elements.length

    // Pre: (elements.length != 0) && (0 <= x < elements.length) && (que != null)
    private static int add(ArrayQueueADT que, int x) {
        return (x + 1) % que.elements.length;
    }
    // Post: (Result == x - 1 && x > 0) || (Result == elements.length && x == 0)

    //Pre: (sz >= 0) && (que != null)
    private static void ensureCapacity(ArrayQueueADT que, int sz) {
        if ((que.elements.length <= sz) || (sz * 3 < que.elements.length)) {
            Object[] temp = new Object[sz * 2 + 1];
            int ind = 0;
            for (int i = que.head; i != que.tail; i = add(que, i)) {
                temp[ind++] = que.elements[i];
            }
            que.elements = temp;
            que.head = 0;
            que.tail = ind;
        }
    }
    // Post: (sz < elem'.length <= sz * 4) && (n' == n) && (a'[i] == a[i] for i = 0...n - 1)

    // Pre: (elem != null) && (que != null)
    public static void enqueue(ArrayQueueADT que, Object elem) {
        assert elem != null;
        ensureCapacity(que, que.size + 1);
        que.size++;
        que.elements[que.tail] = elem;
        que.tail = add(que, que.tail);
    }
    // (n' == n + 1) && (a'[i] == a[i] for i = 0..n - 1) && (a'[n] == elem)

    // Pre: (n > 0) && (que != null)
    public static Object element(ArrayQueueADT que) {
        assert que.size > 0;

        return que.elements[que.head];
    }
    // Post: (n' == n) && (a'[i] == a[i] for i = 0...n - 1) && (Result == a[0])

    // Pre: (n > 0) && (que != null)
    public static Object dequeue(ArrayQueueADT que) {
        assert que.size > 0;
        Object ans = que.elements[que.head];
        que.head = add(que, que.head);
        ensureCapacity(que, --que.size);
        return ans;
    }
    // Post: (n' == n - 1) && (a'[i - 1] == a[i] for i = 1...n - 1) && (Result == a[0])


    // Pre: que != null
    public static int size(ArrayQueueADT que) {
        return que.size;
    }
    // Post: (n' == n) && (a'[i] == a[i] for i = 0...n - 1) && (Result == n)

    // Pre: que != null
    public static boolean isEmpty(ArrayQueueADT que) {
        return que.size == 0;
    }
    // Post: (n' == n) && (a'[i] == a[i] for i = 0...n - 1) && (Result == (n == 0))

    // Pre: que != null
    public static void clear(ArrayQueueADT que) {
        ensureCapacity(que, 1);
        que.size = que.head = que.tail = 0;
    }
    // Post: n == 0

//    public static Object[] toArray(ArrayQueueADT que) {
//        Object result[] = new Object[size(que)];
//        for (int i = 0; i < result.length; i++) {
//            result[i] = que.elements[(que.head + i) % que.elements.length];
//        }
//        return result;
//    }
//

    public static Object[] toArray(ArrayQueueADT q) {
        Object[] res = new Object[size];
        for (int i = 0; i < size; i++) {
            res[i] = element(q);
            enqueue(q, dequeue(q));
        }
        return res;
    }
    // Post: ℝ[n'] && n = n' && ∀ i = 1..n : a[i] = a[i]' && ℝ[i] = a[i]'

    public static String toStr(ArrayQueueADT q) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        Object temp = null;
        for (int i = 0; i < size - 1; i++) {
            temp = dequeue(q);
            sb.append(temp).append(", ");
            enqueue(q, temp);
        }
        if (size != 0) {
            temp = dequeue(q);
            sb.append(temp);
            enqueue(q, temp);
        }
        sb.append("]");
        return sb.toString();
    }
    // Post: ℝ = "[" + (∀ i = 1..n : a[i] + ", ") + "]"
    // && n = n' && ∀ i = 1..n : a[i] = a[i]'
}