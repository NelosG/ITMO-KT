package queue;

import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;
import java.util.TreeMap;

public class MyQueueADTTest {
    ArrayQueueADT queue = new ArrayQueueADT();
    Random r = new Random();
    TreeMap<Integer, Character> map = new TreeMap<>();

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


    void sizeTest(ArrayQueueADT q) throws Exception {
        if (!queue.isEmpty(queue)) throw new Exception("Empty Test Failed(in size test)");
        int size = 0;
        for (int i = 1; i < 1000000; i++) {
            if (i % 3 == 0) {
                size--;
                queue.dequeue(q);
            } else {
                size++;
                queue.enqueue(q, i);
            }
            if (size != queue.size(q)) {
                System.out.println(size + " s    " + queue.size(q) + "   Qs");
                throw new Exception("Size Test Failed");
            }
        }
        for (int i = 0; i < size; i++) {
            queue.dequeue(q);
        }
        if (!queue.isEmpty(q)) throw new Exception("Empty Test Failed(in size test)");
        System.out.println("Size Test Completed");
    }


    void testArrays(ArrayQueueADT q) throws Exception {
        if (!queue.isEmpty(q)) throw new Exception("Empty Test Failed(in arrays test)");
        int[][] ar = new int[10][3];
        int[] temp = new int[3];
        for (int i = 0; i < 10; i++) {
            for (int j = 0; j < 3; j++) {
                temp[j] = rand(1000000);
            }
            ar[i] = temp;
            queue.enqueue(q, temp);
        }
        for (int[] ints : ar) {
            if (!(ints == (int[]) queue.element(q) && ints == (int[]) queue.dequeue(q)))
                throw new Exception("Arrays Test Failed");
        }
        if (!queue.isEmpty(q)) throw new Exception("Empty Test Failed(in arrays test)");
        System.out.println("Arrays Test Completed");
    }

    void testNumbers(ArrayQueueADT q) throws Exception {
        if (!queue.isEmpty(q)) throw new Exception("Empty Test Failed(in numbers test)");
        int[] temp = new int[1000000];
        for (int i = 0; i < 1000000; i++) {
            int h = rand(10000000);
            temp[i] = h;
            queue.enqueue(q, h);
        }
        for (int value : temp) {
            if (!(value == (int) queue.element(q) && value == (int) queue.dequeue(q)))
                throw new Exception("Number Test Failed");
        }
        if (!queue.isEmpty(q)) throw new Exception("Empty Test Failed(in numbers test)");
        System.out.println("Numbers Test Completed");
    }


    void testString(ArrayQueueADT q) throws Exception {
        if (!q.isEmpty(q)) throw new Exception("Empty Test Failed(in string test)");
        buildTreeMap();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 1_000_000; i++) {
            sb.append(rand(28));
        }
        Scanner sc = new Scanner(sb.toString());
        ArrayList<String> ar = new ArrayList<>();
        while (sc.hasNext()) {
            String temp = sc.next();
            ar.add(temp);
            q.enqueue(q, temp);
        }
        sc.close();
        for (String s : ar) {
            if (!(s.equals((String) q.element(q)) && s.equals((String) q.dequeue(q))))
                throw new Exception("String Test Failed");
        }
        if (!q.isEmpty(q)) throw new Exception("Empty Test Failed(in string test)");
        System.out.println("String Test Comleted");
    }

    public static void test(ArrayQueueADT q) throws Exception {
        System.err.println("Start testing ArrayQueueADT");
        MyQueueADTTest test = new MyQueueADTTest();
        test.sizeTest(q);
        test.testArrays(q);
        test.testNumbers(q);
        test.testString(q);
        System.err.println("ArrayQueueADTTest Comleted");
    }
}
