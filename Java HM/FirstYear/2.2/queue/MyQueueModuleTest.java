package queue;

import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;
import java.util.TreeMap;

public class MyQueueModuleTest {
    ArrayQueueModule queue = new ArrayQueueModule();
    Random r = new Random();
    TreeMap<Integer, Character> map = new TreeMap<>();


    void changeEx() {
        this.queue = new ArrayQueueModule();
    }

    int rand(int i) {
        int temp = r.nextInt() % i;
        if (temp < 0) return -temp;
        return temp;
    }

    void buildTreeMap() {
        map.put(0, ' ');
        for (int i = 0; i < 26; i++) {
            map.put(i + 1, (char) ('a' + i));
        }
        map.put(27, ' ');
    }


    void sizeTest() throws Exception {
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in size test)");
        int size = 0;
        for (int i = 1; i < 1000000; i++) {
            changeEx();
            if (i % 3 == 0) {
                size--;
                queue.dequeue();
            } else {
                size++;
                queue.enqueue(i);
            }
            if (size != queue.size()) {
                System.out.println(size + " s    " + queue.size() + "   Qs");
                throw new Exception("Size Test Failed");
            }
        }
        for (int i = 0; i < size; i++) {
            queue.dequeue();
        }
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in size test)");
        System.out.println("Size Test Completed");
    }

    void testArrays() throws Exception {
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in arrays test)");
        int[][] ar = new int[10][3];
        int[] temp = new int[3];
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 3; j++) {
                changeEx();
                temp[j] = rand(1000000);
            }
            ar[i] = temp;
            queue.enqueue(temp);
        }
        for (int[] ints : ar) {
            if (!(ints == (int[]) queue.element() && ints == (int[]) queue.dequeue()))
                throw new Exception("Arrays Test Failed");
        }
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in arrays test)");
        System.out.println("Arrays Test Completed");
    }

    void testNumbers() throws Exception {
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in numbers test)");
        int[] temp = new int[1000000];
        for (int i = 0; i < 1000000; i++) {
            changeEx();
            int h = rand(10000000);
            temp[i] = h;
            queue.enqueue(h);
        }
        for (int value : temp) {
            if (!(value == (int) queue.element() && value == (int) queue.dequeue()))
                throw new Exception("Number Test Failed");
        }
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in numbers test)");
        System.out.println("Numbers Test Completed");
    }

    void testString() throws Exception {
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in string test)");
        buildTreeMap();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 1_000_000; i++) {
            sb.append(rand(28));
        }
        Scanner sc = new Scanner(sb.toString());
        ArrayList<String> ar = new ArrayList<>();
        while (sc.hasNext()) {
            changeEx();
            String temp = sc.next();
            ar.add(temp);
            queue.enqueue(temp);
        }
        sc.close();
        for (String s : ar) {
            if (!(s.equals((String) queue.element()) && s.equals((String) queue.dequeue())))
                throw new Exception("String Test Failed");
        }
        if (!queue.isEmpty()) throw new Exception("Empty Test Failed(in string test)");
        System.out.println("String Test Comleted");
    }

    public static void test() throws Exception {
        System.err.println("Start testing ArrayQueueModule");
        MyQueueModuleTest test = new MyQueueModuleTest();
        test.sizeTest();
        test.testArrays();
        test.testNumbers();
        test.testString();
        System.err.println("ArrayQueueModuleTest Comleted");
    }
}
