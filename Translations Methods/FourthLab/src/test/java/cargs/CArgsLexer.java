package cargs;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import runtime.BaseLexer;

@SuppressWarnings("All")
public class CArgsLexer extends BaseLexer<CArgsLexer.CArgsToken> {
  public CArgsLexer(String input) {
    super(CArgsToken.class, makeSkipTokens(), makeTermTokens(), input);
  }

  protected static Map<String, Pattern> makeSkipTokens() {
    Map<String, Pattern> skipTokens = new HashMap<>();
    skipTokens.put("WS", Pattern.compile("[ \\t\\n\\r]"));
    return skipTokens;
  }

  protected static Map<String, Pattern> makeTermTokens() {
    Map<String, Pattern> termTokens = new HashMap<>();
    termTokens.put("COMMA", Pattern.compile("\\Q,\\E"));
    termTokens.put("STAR", Pattern.compile("\\Q*\\E"));
    termTokens.put("SEMICOLON", Pattern.compile("\\Q;\\E"));
    termTokens.put("NAME", Pattern.compile("[_$a-zA-Z][\\w$]*"));
    return termTokens;
  }

  public enum CArgsToken {
    COMMA,

    STAR,

    SEMICOLON,

    NAME,

    EOF
  }
}
