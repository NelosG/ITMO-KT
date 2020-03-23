package expression.generic;


public class Test {
    public static void main(String[] args) throws Exception {

        Ta<Double> t = new Ta<>();
        t.a((double) 1);
    }

    static class Ta<T extends Number> {
        void a(T a) {
            System.out.println(a.getClass());
        }
    }
}
