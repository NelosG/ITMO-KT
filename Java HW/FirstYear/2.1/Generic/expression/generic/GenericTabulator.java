package expression.generic;

import expression.CommonExpression;
import expression.generic.RIBiD.Bi;
import expression.generic.RIBiD.D;
import expression.generic.RIBiD.I;
import expression.parser.ExpressionParser;

import java.math.BigInteger;

public class GenericTabulator implements Tabulator {
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        CommonExpression expr = new ExpressionParser().parse(expression);
        Object[][][] o = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        for (int i = 0; i <= x2 - x1; ++i) {
            for (int j = 0; j <= y2 - y1; ++j) {
                for (int k = 0; k <= z2 - z1; ++k) {
                    if (mode.equals("i")) {
                        o[i][j][k] = expr.evaluate(new I(), i + x1, j + y1, k + z1);
                    } else if (mode.equals("d")) {
                        o[i][j][k] = expr.evaluate(new D(), (double) (i + x1), (double) (j + y1), (double) (k + z1));
                    } else {
                        o[i][j][k] = expr.evaluate(new Bi(), BigInteger.valueOf(i + x1), BigInteger.valueOf(j + y1), BigInteger.valueOf(k + z1));
                    }
                }
            }
        }
        return o;
    }

}
