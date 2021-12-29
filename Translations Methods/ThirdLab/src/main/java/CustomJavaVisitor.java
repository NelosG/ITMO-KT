import generated.JavaHighlightBaseVisitor;
import generated.JavaHighlightParser;
import org.antlr.v4.runtime.tree.TerminalNode;


public class CustomJavaVisitor extends JavaHighlightBaseVisitor<StringBuilder> {
    private final String nextLine = "<br>" + System.lineSeparator() /*System.lineSeparator()*/;
    private final String space = "&nbsp;" /*" "*/;
    private final String paragraphTag = "p" /*"pre"*/;
    private final String formating = " ".repeat(12) /*""*/;
    @Override
    public StringBuilder visitTerminal(TerminalNode node) {
        return new StringBuilder(node.getText());
    }

    @Override
    public StringBuilder visitForHighlight(JavaHighlightParser.ForHighlightContext ctx) {
        return highLight(visitChildren(ctx), "red");
    }

    @Override
    public StringBuilder visitSpace(JavaHighlightParser.SpaceContext ctx) {
        return new StringBuilder(space);
    }

    @Override
    public StringBuilder visitNewLine(JavaHighlightParser.NewLineContext ctx) {
        return new StringBuilder(nextLine).append(formating);
    }


    @Override
    public StringBuilder visitTab(JavaHighlightParser.TabContext ctx) {
        return new StringBuilder(space.repeat(4));
    }

    @Override
    public StringBuilder visitSpecial(JavaHighlightParser.SpecialContext ctx) {
        String res = switch (visitChildren(ctx).toString()) {
            case "<" -> "&lt;";
            case ">" -> "&gt";
            case "&" -> "&amp;";
            default -> throw new IllegalArgumentException();
        };

        return new StringBuilder(res);
    }

    @Override
    public StringBuilder visitProgram(JavaHighlightParser.ProgramContext ctx) {
        StringBuilder res = new StringBuilder();
        res.append("""
                        <!DOCTYPE html>
                        <html lang="en">
                            <head>
                                <meta charset="UTF-8">
                                <title>JavaHighlight</title>
                            </head>
                            <body>
                        """)
                .append("        <" + paragraphTag + ">\n")
                .append(formating)
                .append(visit(ctx.children.get(0)))
                .append("        </" + paragraphTag + ">\n")
                .append("""
                            </body>
                        </html>
                        """);
        return res;
    }

    @Override
    public StringBuilder visitStringOrCharacter(JavaHighlightParser.StringOrCharacterContext ctx) {
        return highLight(visitChildren(ctx), "green");
    }


    @Override
    public StringBuilder visitAnnotation(JavaHighlightParser.AnnotationContext ctx) {
        return highLight(visitChildren(ctx), "yellow");
    }

    @Override
    protected StringBuilder aggregateResult(StringBuilder aggregate, StringBuilder nextResult) {
        if(aggregate == null) {
            return nextResult;
        }
        if(nextResult != null) {
            aggregate.append(nextResult);
        }
        return aggregate;
    }

    private StringBuilder highLight(StringBuilder sb, String color) {
        return new StringBuilder().append("<span style=\"color:")
                .append(color)
                .append("\">")
                .append(sb)
                .append("</span>");
    }
}
