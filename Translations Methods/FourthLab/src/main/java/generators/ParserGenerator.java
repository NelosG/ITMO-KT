package generators;

import builder.Builder;
import com.squareup.javapoet.*;
import types.*;

import javax.lang.model.element.Modifier;
import java.util.*;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ParserGenerator implements BaseGenerator {
    public final Map<String, Set<String>> first;
    public final Map<String, Set<String>> follow;
    private final Map<String, Pattern> termToken;
    private final Map<String, Rule> rules;
    private final String startRule;
    private final String parserName;
    private final String lexerName;
    private final String tokenName;

    public ParserGenerator(Builder builder) {
        termToken = new HashMap<>(Map.copyOf(builder.getTermTokens()));
        termToken.put("EPS", Pattern.compile(Pattern.quote("")));
        rules = new HashMap<>(Map.copyOf(builder.getRules()));
        startRule = builder.getStartRule();
        parserName = builder.getParserName();
        lexerName = builder.getLexerName();
        tokenName = builder.getTokenName();
        first = first();
        follow = follow();
    }


    @Override
    public JavaFile.Builder generate(String filePackage) {
        String packageName = filePackage.toLowerCase();

        TypeSpec.Builder res = TypeSpec.classBuilder(parserName)
                .addModifiers(Modifier.PUBLIC)
                .addAnnotation(
                        AnnotationSpec.builder(SuppressWarnings.class)
                                .addMember("value", "$L", "\"All\"")
                                .build()
                )
                .addField(
                        FieldSpec.builder(
                                ClassName.get(packageName, lexerName),
                                "lexer",
                                Modifier.PRIVATE, Modifier.FINAL).build()
                )
                .addMethod(
                        MethodSpec.constructorBuilder()
                                .addModifiers(Modifier.PUBLIC)
                                .addParameter(ClassName.get(packageName, lexerName), "lexer")
                                .addCode(
                                        CodeBlock.builder()
                                                .addStatement("this.lexer = lexer")
                                                .build()
                                )
                                .build()
                )
                .addMethod(
                        parseFunction()
                                .build()
                )
                .addMethod(
                        processTokenFunction(packageName)
                                .build()
                );
        addFunctions(res);
        return JavaFile.builder(packageName, res.build());
    }


    private Map<String, Set<String>> first() {
        Map<String, Set<String>> first = new HashMap<>();

        termToken.forEach((key, v) -> {
            Set<String> set = new HashSet<>();
            set.add(key);
            first.put(key, set);
        });

        rules.forEach((name, rule) -> {
            first.put(name, new HashSet<>());

            if (rule.productions().stream().anyMatch(it ->
                    {
                        if (it.extendedProduction().get(0) instanceof Element element) {
                            return Objects.equals(element.getName(), "EPS");
                        }
                        return false;
                    }
            )) {
                first.get(name).add("EPS");
            }
        });
        boolean changed = true;

        while (changed) {
            changed = false;
            for (var vk : rules.entrySet()) {
                String ruleName = vk.getKey();
                Rule rule = vk.getValue();
                for (Production production : rule.productions()) {
                    for (ExtendedElement maybeElement : production.extendedProduction()) {
                        if (maybeElement instanceof Element element) {
                            String elementName = element.getName();

                            Set<String> temp = first.get(elementName) != null ?
                                    new HashSet<>(first.get(elementName)) :
                                    new HashSet<>();

                            temp.remove("EPS");
                            first.computeIfAbsent(ruleName, k -> new HashSet<>());

                            changed = changed || first.get(ruleName).addAll(temp);

                            if (first.get(elementName) != null &&
                                    !first.get(elementName).contains("EPS")) {
                                break;
                            }
                            if (maybeElement == production.extendedProduction().get(production.extendedProduction().size() - 1)) {
                                changed = changed || first.get(ruleName).add("EPS");
                            }
                        }
                    }
                }
            }
        }

        return first;
    }

    private Map<String, Set<String>> follow() {
        Map<String, Set<String>> follow = new HashMap<>();

        rules.forEach((key, v) -> follow.put(key, new HashSet<>()));

        follow.computeIfAbsent(startRule, k -> new HashSet<>());
        follow.get(startRule).add("EOF");

        boolean changed = true;

        while (changed) {
            changed = false;
            for (var vk : rules.entrySet()) {
                String name = vk.getKey();
                Rule rule = vk.getValue();
                for (Production production : rule.productions()) {
                    for (int i = 0; i < production.extendedProduction().size() - 1; i++) {
                        if (production.extendedProduction().get(i) instanceof NonTerm element &&
                                production.extendedProduction().get(i + 1) instanceof Element next) {


                            Set<String> temp = first.get(next.getName()) != null ?
                                    new HashSet<>(first.get(next.getName())) :
                                    new HashSet<>();
                            temp.remove("EPS");

                            changed = changed || follow.get(element.getName()).addAll(temp);

                            if (first.get(next.getName()) != null &&
                                    first.get(next.getName()).contains("EPS")) {
                                changed = changed || follow.get(element.getName()).addAll(follow.get(name));
                            }
                        }
                    }

                    ExtendedElement maybeElement =
                            production.extendedProduction()
                                    .get(production.extendedProduction().size() - 1);

                    if (maybeElement instanceof NonTerm element) {
                        changed = changed ||
                                follow.get(element.getName())
                                        .addAll(follow.get(name));
                    }
                }
            }
        }

        return follow;
    }

    private MethodSpec.Builder parseFunction() {
        MethodSpec.Builder meth =
                MethodSpec.methodBuilder("parse")
                        .addModifiers(Modifier.PUBLIC);

        String returnType = rules.get(startRule).returnType();

        if (returnType != null) {
            meth.returns(getType(returnType));
        }
        return meth.addStatement("return " + startRule + "()");
    }

    private MethodSpec.Builder processTokenFunction(String filePackage) {
        return MethodSpec.methodBuilder("processToken")
                .addParameter(
                        ClassName.get(filePackage, lexerName + "." + tokenName),
                        "token"
                )
                .returns(String.class)
                .addStatement(
                        "if (lexer.getToken() != token) throw new IllegalArgumentException()"
                )
                .addStatement("var res = lexer.getTokenValue()")
                .addStatement("lexer.nextToken()")
                .addStatement("return res");

    }

    private TypeSpec.Builder addFunctions(TypeSpec.Builder builder) {
        List<MethodSpec> methods = new ArrayList<>();
        rules.forEach((key, value) -> {
            MethodSpec.Builder method = MethodSpec.methodBuilder(key);
            methods.add(
                    addFunction(method, key, value)
                            .addModifiers(Modifier.PRIVATE)
                            .build()
            );
        });
        builder.addMethods(methods);
        return builder;
    }

    private MethodSpec.Builder addFunction(MethodSpec.Builder builder, String name, Rule rule) {
        addParameters(builder, rule);
        if (rule.returnType() != null) {
            builder.returns(getType(rule.returnType()));
        }
        addFunctionBody(builder, name);
        return builder;
    }

    private MethodSpec.Builder addFunctionBody(MethodSpec.Builder builder, String name) {
        builder.beginControlFlow("switch (lexer.getToken())");
        addCases(builder, name);
        return builder.endControlFlow();
    }

    private MethodSpec.Builder addCases(MethodSpec.Builder builder, String ruleName) {
        for (Production production : rules.get(ruleName).productions()) {
            Set<String> productionFirst = productionFirst(production, ruleName);

            builder.beginControlFlow(
                    String.format("case %s ->",
                            productionFirst.stream()
                                    .filter(Predicate.not(String::isEmpty))
                                    .collect(Collectors.joining(", "))
                    )
            );
            addCaseBody(builder, production);
            builder.endControlFlow();
        }

        return builder.beginControlFlow("default ->")
                .addStatement("throw new IllegalArgumentException()")
                .endControlFlow();
    }

    private MethodSpec.Builder addCaseBody(MethodSpec.Builder builder, Production production) {
        HashMap<String, Integer> variables = new HashMap<>();

        for (ExtendedElement maybeExtended : production.extendedProduction()) {
            if (maybeExtended instanceof Element element) {
                variables.putIfAbsent(element.getName(), -1);
                variables.computeIfPresent(element.getName(), ((key, value) -> value + 1));

                Integer cnt = variables.get(element.getName());

                String variableName = element.getName() +
                        (cnt == 0 ? "" : cnt);


                if (element instanceof NonTerm nonTerm) {
                    builder.addStatement(
                            "var " + variableName + " = " + element.getName() + "(" +
                                    String.join(",", nonTerm.getCallAttributes()) +
                                    ")"
                    );
                } else if (!element.getName().equals("EPS")) {
                    builder.addStatement("var " + variableName +
                            " = processToken(" +
                            lexerName + "." + tokenName + "." + element.getName() +
                            ")");
                }
            } else if (maybeExtended instanceof Code code) {
                builder.addStatement(code.code());
            }
        }

        if (production.returnExpression() != null) {
            builder.addStatement("return " + production.returnExpression());
        }
        return builder;
    }

    private MethodSpec.Builder addParameters(MethodSpec.Builder builder, Rule rule) {
        rule.arguments()
                .forEach(argument ->
                        builder.addParameter(getType(argument.type()), argument.name())
                );
        return builder;
    }

    private Set<String> productionFirst(Production production, String name) {
        HashSet<String> productionFirst = new HashSet<>();

        for (var maybeElement : production.extendedProduction()) {
            if (maybeElement instanceof Element element) {
                Set<String> elementFirst = first.get(element.getName());
                if (elementFirst != null) {
                    productionFirst.addAll(elementFirst);

                    if (!elementFirst.contains("EPS")) {
                        productionFirst.remove("EPS");
                        break;
                    }
                }
            }
        }

        if (productionFirst.contains("EPS")) {
            productionFirst.addAll(follow.get(name));
            productionFirst.remove("EPS");
        }
        return productionFirst;
    }

    private Class<?> getType(String type) {
        switch (type) {
            case "Int" -> {
                return Integer.class;
            }
            case "Double" -> {
                return Double.class;
            }
            case "Node" -> {
                return Node.class;
            }
            case "String" -> {
                return String.class;
            }
            default -> {
                try {
                    return Class.forName(type);
                } catch (ClassNotFoundException e) {
                    throw new IllegalArgumentException(e.getMessage());
                }
            }
        }
    }

    public String getParserName() {
        return parserName;
    }
}
