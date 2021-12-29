package generators;

import builder.Builder;
import com.squareup.javapoet.*;

import javax.lang.model.element.Modifier;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

public class LexerGenerator implements BaseGenerator {
    private final Map<String, Pattern> skipTokens;
    private final Map<String, Pattern> termTokens;
    private final String lexerName;
    private final String tokenName;

    public LexerGenerator(Builder builder) {
        skipTokens = builder.getSkipTokens();
        termTokens = builder.getTermTokens();
        lexerName = builder.getLexerName();
        tokenName = builder.getTokenName();
    }

    public String getLexerName() {
        return lexerName;
    }

    public JavaFile.Builder generate(String filePackage) {
        return JavaFile.builder(filePackage.toLowerCase(), lexerDeclaration(filePackage.toLowerCase())
                        .addAnnotation((AnnotationSpec.builder(SuppressWarnings.class)
                                .addMember("value", "$L", "\"All\"")
                                .build()))
                        .addMethod(tokenMap("skipTokens", skipTokens))
                        .addMethod(tokenMap("termTokens", termTokens))
                        .addType(tokenEnum(termTokens)
                                .addModifiers(Modifier.PUBLIC, Modifier.STATIC)
                                .build())
                        .build())
                .skipJavaLangImports(true);
    }

    private TypeSpec.Builder tokenEnum(Map<String, Pattern> map) {
        TypeSpec.Builder typeSpec = TypeSpec.enumBuilder(tokenName);
        map.keySet()
                .forEach(typeSpec::addEnumConstant);
        typeSpec.addEnumConstant("EOF");
        return typeSpec;
    }

    private MethodSpec tokenMap(String name, Map<String, Pattern> map) {
        ParameterizedTypeName type = ParameterizedTypeName.get(ClassName.get(Map.class),
                ClassName.get(String.class),
                ClassName.get(Pattern.class));

        MethodSpec.Builder res =
                MethodSpec.methodBuilder("make" + Character.toUpperCase(name.charAt(0)) + name.substring(1))
                        .returns(type)
                        .addModifiers(Modifier.PROTECTED, Modifier.STATIC)
                        .addStatement("$T $T = new $T<>()",
                                type,
                                TypeVariableName.get(name),
                                ClassName.get(HashMap.class));


        map.forEach((key, value) -> {
            res.addStatement("$T.put($S, Pattern.compile($S))",
                    TypeVariableName.get(name),
                    key,
                    value);
        });
        return res.addStatement("return " + name).build();
    }

    private TypeSpec.Builder lexerDeclaration(String filePackage) {
        return TypeSpec.classBuilder(lexerName)
                .addModifiers(Modifier.PUBLIC)
                .addMethod(MethodSpec.constructorBuilder()
                        .addModifiers(Modifier.PUBLIC)
                        .addParameter(String.class, "input")
                        .addCode(CodeBlock.builder()
                                .addStatement(
                                        "super(" + tokenName + ".class, " + "makeSkipTokens(), makeTermTokens(), input)")
                                .build())
                        .build())
                .superclass(ParameterizedTypeName.get(
                        ClassName.get("runtime", "BaseLexer"),
                        ClassName.get(filePackage, lexerName + "." + tokenName)
                ));
    }
}
