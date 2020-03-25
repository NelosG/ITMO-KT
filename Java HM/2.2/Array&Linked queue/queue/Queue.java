package queue;

public interface Queue {

    // Pre: element != null
    void enqueue(Object element);
    // (n' == n + 1) && elements of queue haven't moved  && last element == "element"


    // Pre: n > 0
    Object element();
    // Post: (n' == n) && elements of queue haven't moved && returned first element in queue


    // Pre: n > 0
    Object dequeue();
    // Post: (n' == n - 1) && elements of queue have moved by one && first element returned and deleted from queue


    int size();
    // Post: (n' == n) && elements of queue haven't moved && returned quantity elements of queue


    boolean isEmpty();
    // Post: (n' == n) && elements of queue haven't moved && returned n == 0


    void clear();
    // Post: n == 0


    Object[] toArray();
    // Post: ℝ[n'] && n = n' && ∀ i = 1..n : a[i] = a[i]' && ℝ[i] = a[i]'

}
