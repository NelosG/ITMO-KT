package runtime;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class BaseLexer<T extends Enum<T>> {
    protected final Class<T> tokenClass;
    protected final Map<String, Pattern> skipTokens;
    protected final String input;
    protected final Map<T, Pattern> termTokens;
    protected int position = 0;
    private String tokenValue;
    private T token;

    public BaseLexer(Class<T> tokenClass,
                     Map<String, Pattern> skipTokens,
                     Map<String, Pattern> termTokens,
                     String input) {
        this.tokenClass = tokenClass;
        this.skipTokens = skipTokens;
        this.input = input;
        this.termTokens = new HashMap<>();
        termTokens.forEach(
                (key, value) -> {
                    this.termTokens.put(
                            java.lang.Enum.valueOf(
                                    tokenClass,
                                    key
                            ),
                            value);
                }
        );
        nextToken();
    }

    public String getTokenValue() {
        return tokenValue;
    }

    public T getToken() {
        return token;
    }


    public T nextToken() {

        boolean skipped = true;
        while (skipped) {
            skipped = false;
            for (var vk : skipTokens.entrySet()) {
                Matcher matcher = vk.getValue().matcher(input);

                if (matcher.find(position) && matcher.start() == position) {
                    position = matcher.toMatchResult().end();
                    skipped = true;
                }
            }
        }
        if (position >= input.length()) {
            token = java.lang.Enum.valueOf(tokenClass, "EOF");
            tokenValue = "";

            return token;
        }
        Pair<T, MatchResult> matched = null;

        for (var vk : termTokens.entrySet()) {
            T key = vk.getKey();
            Matcher matcher = vk.getValue().matcher(input);


            if (matcher.find(position) && matcher.start() == position) {
                MatchResult result = matcher.toMatchResult();
                if (matched != null) {
                    throw new IllegalTokenException(
                            String.format("String started at %s matched ambiguous tokens: %s, %s",
                                    position + 1,
                                    matched.first,
                                    key)
                    );
                }
                matched = new Pair<>(key, result);
            }
        }

        if (matched == null) {
            throw new IllegalTokenException("Can't tokenize substring starting at position " + (position + 1));
        }

        position = matched.second.end();
        tokenValue = matched.second.group();
        token = matched.first;

        return token;
    }
}
