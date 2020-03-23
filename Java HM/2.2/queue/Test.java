package queue;

public class Test {
    public static void main(String[] args) throws Exception {
        long start = System.currentTimeMillis();
        MyQueueTest.test();
        MyQueueModuleTest.test();
        MyQueueADTTest.test(new ArrayQueueADT());
        System.err.println("Time: " + (System.currentTimeMillis() - start) + " ms");
    }
}
