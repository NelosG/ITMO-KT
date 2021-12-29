package builder;

import generated.GrammarBaseListener;
import generated.GrammarParser;
import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import types.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Builder extends GrammarBaseListener {

    private final Map<String, Pattern> skipTokens = new HashMap<>();
    private final Map<String, Pattern> termTokens = new HashMap<>();
    private final Map<String, Rule> rules = new HashMap<>();
    private final String lexerName;
    private final String parserName;
    private final String tokenName;
    private String startRule;

    public Builder(String grammarName) {
        if (grammarName.isBlank()) {
            throw new BuilderException("Wrong grammar name");
        }
        String capitalized = Character.toUpperCase(grammarName.charAt(0)) +
                (grammarName.length() > 2 ? grammarName.substring(1) : "");

        lexerName = capitalized + "Lexer";
        parserName = capitalized + "Parser";
        tokenName = capitalized + "Token";
    }

    @Override
    public void exitStartRule(GrammarParser.StartRuleContext ctx) {
        startRule = ctx.ParserIdentifier().getText();
    }

    @Override
    public void exitSkip(GrammarParser.SkipContext ctx) {
        skipTokens.putIfAbsent(ctx.LexerIdentifier().getText(), toRegexp(ctx.terminals()));
    }

    @Override
    public void exitTerm(GrammarParser.TermContext ctx) {
        termTokens.putIfAbsent(ctx.LexerIdentifier().getText(), toRegexp(ctx.terminals()));
    }


    private Pattern toRegexp(GrammarParser.TerminalsContext terminals) {
        TerminalNode regexp = terminals.Regexp();
        if (regexp == null || regexp.getText() == null) {
            return NotRegex(terminals);
        }
        return Pattern.compile(extract(regexp, "'", "'"));
    }

    private Pattern NotRegex(GrammarParser.TerminalsContext terminals) {
        return Pattern.compile(Pattern.quote(extract(terminals.String(), "\"", "\"")));
    }


    @Override
    public void exitGrammarParserRule(GrammarParser.GrammarParserRuleContext ctx) {
        String name = ctx.ParserIdentifier().getText();
        List<Arg> arguments = extractArguments(ctx.arguments() != null ?
                ctx.arguments().argument() :
                null);
        String returnType = ctx.returnType() != null ?
                ctx.returnType().type() != null ?
                        ctx.returnType().type().getText() :
                        null :
                null;

        rules.putIfAbsent(name,
                new Rule(name,
                        arguments,
                        returnType,
                        extractProductions(ctx.productions().production())
                ));
    }

    private List<Arg> extractArguments(List<GrammarParser.ArgumentContext> arguments) {
        return arguments == null ? List.of() :
                arguments.stream().map(
                        argument ->
                                new Arg(argument.argName().getText(), argument.type().getText())
                ).collect(Collectors.toList());
    }

    private List<Production> extractProductions(List<GrammarParser.ProductionContext> productions) {
        List<Production> productionList = new ArrayList<>();

        for (GrammarParser.ProductionContext production : productions) {
            List<ExtendedElement> extendedElements = new ArrayList<>();

            boolean isEmpty = true;

            for (GrammarParser.ExtendedElementsContext extendedElement : production.extendedElements()) {
                if (extendedElement.CODE() != null) {
                    extendedElements.add(
                            new Code(
                                    extractCode(extendedElement.CODE())
                            ));
                } else if (extendedElement.MANYLINECODE() != null) {
                    extendedElements.add(
                            new Code(
                                    extractManyLineCode(extendedElement.MANYLINECODE())
                            ));
                } else {
                    isEmpty = false;
                    extendedElements.add(
                            extractElement(extendedElement.element())
                    );
                }
            }

            if (isEmpty) {
                extendedElements.add(new Term("EPS"));
            }

            productionList.add(new Production(extendedElements, extractReturn(production.returnExpression())));
        }

        return productionList;
    }

    private ExtendedElement extractElement(GrammarParser.ElementContext element) {
        if (element.LexerIdentifier() != null) {
            return new Term(element.LexerIdentifier().getText());
        } else {
            return new NonTerm(
                    element.ParserIdentifier().getText(),
                    extractCallAttributes(element.callAttributes() != null ?
                            element.callAttributes().callAttribute() : null)
            );
        }
    }

    private String extractReturn(GrammarParser.ReturnExpressionContext ret) {
        if (ret != null) {
            TerminalNode code = ret.CODE();
            if (code != null) {
                return extractCode(code);
            } else {
                TerminalNode manyLineCode = ret.MANYLINECODE();
                if (manyLineCode != null) {
                    return extractManyLineCode(manyLineCode);
                } else {
                    String returnVal = ret.getText();
                    return returnVal.substring(1, returnVal.length() - 1);
                }
            }
        }
        return null;
    }

    private String extractManyLineCode(TerminalNode manyLineCode) {
        return extract(manyLineCode, "`{", "}`");
    }

    private String extractCode(TerminalNode code) {
        return extract(code, "{", "}");
    }

    private String extract(TerminalNode code, String prefix, String suffix) {
        String temp = code.getText();
        if (temp.startsWith(prefix) && temp.endsWith(suffix)) {
            return temp.substring(prefix.length(), temp.length() - suffix.length());
        }
        throw new BuilderException("Extraction FAILED");
    }

    private List<String> extractCallAttributes(List<GrammarParser.CallAttributeContext> attributes) {
        return attributes == null ? List.of() :
                attributes.stream().map(RuleContext::getText).collect(Collectors.toList());
    }

    public Map<String, Pattern> getSkipTokens() {
        return skipTokens;
    }

    public Map<String, Pattern> getTermTokens() {
        return termTokens;
    }

    public Map<String, Rule> getRules() {
        return rules;
    }

    public String getStartRule() {
        return startRule;
    }

    public String getLexerName() {
        return lexerName;
    }

    public String getParserName() {
        return parserName;
    }

    public String getTokenName() {
        return tokenName;
    }
}
