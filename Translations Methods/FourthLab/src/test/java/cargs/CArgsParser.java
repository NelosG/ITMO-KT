package cargs;

import java.lang.String;
import java.lang.SuppressWarnings;
import types.Node;

@SuppressWarnings("All")
public class CArgsParser {
  private final CArgsLexer lexer;

  public CArgsParser(CArgsLexer lexer) {
    this.lexer = lexer;
  }

  public Node parse() {
    return s();
  }

  String processToken(CArgsLexer.CArgsToken token) {
    if (lexer.getToken() != token) throw new IllegalArgumentException();
    var res = lexer.getTokenValue();
    lexer.nextToken();
    return res;
  }

  private Node next() {
    switch (lexer.getToken()) {
      case NAME -> {
        var s = s();
        return new Node("Next").add(s);
      }
      case EOF -> {
        return null;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node s() {
    switch (lexer.getToken()) {
      case NAME -> {
        var type = type();
        var d = d();
        var l = l();
        var next = next();
        return  new Node("S").add(type, d, l , next) ;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node d() {
    switch (lexer.getToken()) {
      case STAR, NAME -> {
        var t = t();
        var k = k();
        return  new Node("D").add(t, k) ;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node t() {
    switch (lexer.getToken()) {
      case STAR -> {
        var STAR = processToken(CArgsLexer.CArgsToken.STAR);
        var t = t();
        return  new Node("T").add(new Node(STAR), t) ;
      }
      case NAME -> {
        var var = var();
        return new Node("T").add(var);
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node var() {
    switch (lexer.getToken()) {
      case NAME -> {
        var NAME = processToken(CArgsLexer.CArgsToken.NAME);
        return  new Node("Variable").add(new Node(NAME)) ;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node v() {
    switch (lexer.getToken()) {
      case SEMICOLON -> {
        var l = l();
        return  new Node("V").add(l) ;
      }
      case EOF, NAME -> {
        return null;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node k() {
    switch (lexer.getToken()) {
      case COMMA -> {
        var COMMA = processToken(CArgsLexer.CArgsToken.COMMA);
        var d = d();
        return  new Node("K").add(new Node(COMMA), d) ;
      }
      case SEMICOLON -> {
        return null;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node type() {
    switch (lexer.getToken()) {
      case NAME -> {
        var NAME = processToken(CArgsLexer.CArgsToken.NAME);
        return  new Node("Type").add(new Node(NAME)) ;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }

  private Node l() {
    switch (lexer.getToken()) {
      case SEMICOLON -> {
        var SEMICOLON = processToken(CArgsLexer.CArgsToken.SEMICOLON);
        var v = v();
        return  new Node("L").add(new Node(SEMICOLON), v) ;
      }
      default -> {
        throw new IllegalArgumentException();
      }
    }
  }
}
