package cargs;

import cargs.tree.Tree;

public class TestCArgsParser {
    public static void main(String[] args) {
        String text = """
                int a, **b
                        ,
                    *
                c;""";
        System.out.println(Tree.print(new CArgsParser(new CArgsLexer(text)).parse()));
    }
}
