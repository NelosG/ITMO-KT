import java.io.*;
import java.util.*;

public class N {

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new FileInputStream("linkedmap.in"));
        FileWriter writer = new FileWriter("linkedmap.out");

        HasLinkedMap set = new HasLinkedMap();
        while (scanner.hasNext()) {
            String cmd = scanner.next();
            switch (cmd) {
                case "put":
                    set.put(scanner.next(), scanner.next());
                    break;
                case "delete":
                    set.remove(scanner.next());
                    break;
                case "get":
                    writer.write(set.get(scanner.next()) + '\n');
                    break;
                case "prev":
                    writer.write(set.prev(scanner.next()) + '\n');
                    break;
                case "next":
                    writer.write(set.next(scanner.next()) + '\n');
                    break;
            }
        }

        scanner.close();
        writer.close();
    }


    static class HasLinkedMap {

        int mas = 1000;
        LinkedList<Node>[] array = new LinkedList[mas];
        int size = 0;
        Node last = null;
        Node current = null;

        HasLinkedMap() {
            for (int i = 0; i < mas; i++)
                array[i] = new LinkedList<>();
        }

        void put(String key, String value) {
            int hash = h(key);
            for (int i = 0; i < array[hash].size(); i++) {
                if (array[hash].get(i).key.equals(key)) {
                    Node Current = new Node(key, value);
                    if (array[hash].get(i).next != null) Current.next = array[hash].get(i).next;
                    if (array[hash].get(i).prev != null) Current.prev = array[hash].get(i).prev;
                    if (Current.next != null) Current.next.prev = Current;
                    if (Current.prev != null) Current.prev.next = Current;
                    if (last.key.equals(Current.key)) last = Current;
                    array[hash].set(i, Current);
                    return;
                }
            }
            current = new Node(key, value);
            if (last != null)
                last.next = current;
            current.prev = last;
            last = current;
            array[hash].add(current);
            size++;
        }

        String get(String key) {
            for (Node node : array[h(key)])
                if (node.key.equals(key))
                    return node.value;
            return "none";
        }

        void remove(String key) {
            int hash = h(key);
            for (int i = 0; i < array[hash].size(); i++) {
                if (array[hash].get(i).key.equals(key)) {
                    Node Current = array[hash].get(i);
                    if (Current.prev != null) Current.prev.next = Current.next;
                    if (Current.next != null) Current.next.prev = Current.prev;
                    if (last.key.equals(Current.key)) last = Current.prev;
                    array[hash].remove(i);
                    size--;
                    break;
                }
            }
        }

        String next(String key) {
            int hash = h(key);
            for (int i = 0; i < array[hash].size(); i++) {
                if (array[hash].get(i).key.equals(key)) {
                    if (array[hash].get(i).next != null) return array[hash].get(i).next.value;
                    else return "none";
                }
            }
            return "none";
        }

        String prev(String key) {
            int hash = h(key);
            for (int i = 0; i < array[hash].size(); i++)
                if (array[hash].get(i).key.equals(key)) {
                    if (array[hash].get(i).prev != null) return array[hash].get(i).prev.value;
                    else return "none";
                }
            return "none";
        }

        int h(String key) {
            int hash = 0;
            byte[] bytes = key.getBytes();
            for (byte v : bytes)
                hash = 31 * hash + (v % 10);
            hash = Math.abs(hash);
            return hash % mas;
        }
    }

    static class Node {
        String key, value;
        Node next, prev;

        Node(String key, String value) {
            this.key = key;
            this.value = value;
            this.next = null;
            this.prev = null;
        }
    }
}