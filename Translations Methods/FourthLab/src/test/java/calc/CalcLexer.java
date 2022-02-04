package calc;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import runtime.BaseLexer;

@SuppressWarnings("All")
public class CalcLexer extends BaseLexer<CalcLexer.CalcToken> {
  public CalcLexer(String input) {
    super(CalcToken.class, makeSkipTokens(), makeTermTokens(), input);
  }

  protected static Map<String, Pattern> makeSkipTokens() {
    Map<String, Pattern> skipTokens = new HashMap<>();
    skipTokens.put("WS", Pattern.compile("[ \\t\\n\\r]"));
    return skipTokens;
  }

  protected static Map<String, Pattern> makeTermTokens() {
    Map<String, Pattern> termTokens = new HashMap<>();
    termTokens.put("DIV", Pattern.compile("\\Q/\\E"));
    termTokens.put("LFPAREN", Pattern.compile("\\Q{\\E"));
    termTokens.put("NUMBER", Pattern.compile("[0-9]+"));
    termTokens.put("MUL", Pattern.compile("\\Q*\\E"));
    termTokens.put("LPAREN", Pattern.compile("\\Q(\\E"));
    termTokens.put("RPAREN", Pattern.compile("\\Q)\\E"));
    termTokens.put("POINT", Pattern.compile("\\Q.\\E"));
    termTokens.put("RFPAREN", Pattern.compile("\\Q}\\E"));
    termTokens.put("PLUS", Pattern.compile("\\Q+\\E"));
    termTokens.put("MINUS", Pattern.compile("\\Q-\\E"));
    return termTokens;
  }

  public enum CalcToken {
    DIV,

    LFPAREN,

    NUMBER,

    MUL,

    LPAREN,

    RPAREN,

    POINT,

    RFPAREN,

    PLUS,

    MINUS,

    EOF
  }
}
