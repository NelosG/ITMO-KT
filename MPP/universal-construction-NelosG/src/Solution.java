/**
 * @author Pushkarev Gleb
 */
public class Solution implements AtomicCounter {
    // объявите здесь нужные вам поля
    private final Node root = new Node(0);
    private final ThreadLocal<Node> last = ThreadLocal.withInitial(() -> root);

    public int getAndAdd(int x) {
        int old= last.get().value;
        Node node = new Node(old + x);
        last.set(last.get().next.decide(node));
        while (node != last.get()) {
            old = last.get().value;
            node = new Node(old + x);
            last.set(last.get().next.decide(node));
        }
        return old;
    }

    // вам наверняка потребуется дополнительный класс
    private static class Node {
        public Node(int x) {
            value = x;
        }
        final int value;
        final Consensus<Node> next = new Consensus<>();
    }
}
