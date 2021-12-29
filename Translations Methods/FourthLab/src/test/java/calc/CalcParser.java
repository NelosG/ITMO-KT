package calc;

import java.lang.Double;
import java.lang.String;
import java.lang.SuppressWarnings;

@SuppressWarnings("All")
public class CalcParser {
  private final CalcLexer lexer;

  public CalcParser(CalcLexer lexer) {
    this.lexer = lexer;
  }

  public Double parse() {
    return expr();
  }

  String processToken(CalcLexer.CalcToken token) {
    if (lexer.getToken() != token) throw new IllegalArgumentException();
    var res = lexer.getTokenValue();
    lexer.nextToken();
    return res;
  }

  private Double exprPrime(Double t) {
    switch (lexer.getToken()) {
      case PLUS -> {
        var PLUS = processToken(CalcLexer.CalcToken.PLUS);
        var term = term();
         Double res = t + term ;
        var exprPrime = exprPrime(res);
        return exprPrime;
      }
      case MINUS -> {
        var MINUS = processToken(CalcLexer.CalcToken.MINUS);
        var term = term();
         Double res = t - term ;
        var exprPrime = exprPrime(res);
        return exprPrime;
      }
      case RPAREN, EOF, RFPAREN -> {
        return t;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Double fact() {
    switch (lexer.getToken()) {
      case LPAREN -> {
        var LPAREN = processToken(CalcLexer.CalcToken.LPAREN);
        var expr = expr();
        var RPAREN = processToken(CalcLexer.CalcToken.RPAREN);
        return expr;
      }
      case LFPAREN -> {
        var LFPAREN = processToken(CalcLexer.CalcToken.LFPAREN);
        var expr = expr();
        var RFPAREN = processToken(CalcLexer.CalcToken.RFPAREN);
        return  expr % 1.0 ;
      }
      case NUMBER -> {
        var NUMBER = processToken(CalcLexer.CalcToken.NUMBER);
        var afterPoint = afterPoint(NUMBER);
        return afterPoint;
      }
      case MINUS -> {
        var MINUS = processToken(CalcLexer.CalcToken.MINUS);
        var fact = fact();
        return  fact * -1 ;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Double expr() {
    switch (lexer.getToken()) {
      case LFPAREN, NUMBER, LPAREN, MINUS -> {
        var term = term();
        var exprPrime = exprPrime(term);
        return exprPrime;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Double term() {
    switch (lexer.getToken()) {
      case LFPAREN, NUMBER, LPAREN, MINUS -> {
        var fact = fact();
        var termPrime = termPrime(fact);
        return termPrime;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Double afterPoint(String first) {
    switch (lexer.getToken()) {
      case POINT -> {
        var POINT = processToken(CalcLexer.CalcToken.POINT);
        var NUMBER = processToken(CalcLexer.CalcToken.NUMBER);
        return  Double.parseDouble(first + POINT + NUMBER) ;
      }
      case DIV, MUL, RPAREN, EOF, RFPAREN, PLUS, MINUS -> {
        return  Double.parseDouble(first) ;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Double termPrime(Double f) {
    switch (lexer.getToken()) {
      case MUL -> {
        var MUL = processToken(CalcLexer.CalcToken.MUL);
        var fact = fact();
         Double res = f * fact ;
        var termPrime = termPrime(res);
        return termPrime;
      }
      case DIV -> {
        var DIV = processToken(CalcLexer.CalcToken.DIV);
        var fact = fact();
         Double res = f / fact ;
        var termPrime = termPrime(res);
        return termPrime;
      }
      case RPAREN, EOF, RFPAREN, PLUS, MINUS -> {
        return f;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }
}
