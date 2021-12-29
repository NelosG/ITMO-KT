package parser;

import tokenizer.Tokenizer;
import tokenizer.TokenizerException;
import tree.Node;

import static tokenizer.Token.*;


public class Parser {
    private final String INCORRECT_TOKEN_EXPECTED = "Incorrect Token: expected one of ";
    private Tokenizer tokenizer;

    public Node parse(String text) throws ParserException {
        tokenizer = new Tokenizer(text);
        try {
            if (tokenizer.token() == null) {
                tokenizer.nextToken();
            }
        } catch (TokenizerException e) {
            throw new ParserException("Can't read first token", e);
        }
        return S();
    }

    private Node S() throws ParserException {
        // S -> Type D L Next
        if (tokenizer.token() == TYPE) {
            return new Node("S").add(Type(), D(), L(), Next());
        }
        throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<Type> ");
    }

    private Node Next() throws ParserException {
        switch (tokenizer.token()) {
            // Next -> S
            case TYPE -> {
                return new Node("Next").add(S());
            }
            // Next -> eps
            case END -> {
                return null;
            }
            default -> throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<Type | END> ");
        }
    }


    private Node L() throws ParserException {
        try {
            // L -> ; V
            if (tokenizer.token() == SEMICOLON) {
                tokenizer.nextToken(); // we “eat“ ";" token
                return new Node("L").add(new Node("\";\""), V());
            }
            throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<';'>");
        } catch (TokenizerException e) {
            throw new ParserException(e);
        }
    }

    private Node V() throws ParserException {
        switch (tokenizer.token()) {
            // V -> L
            case SEMICOLON -> {
                return new Node("V").add(L());
            }
            // V -> eps
            case TYPE, END -> {
                return null;
            }
            default -> throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<';' | Type, END>");
        }
    }

    private Node D() throws ParserException {
        switch (tokenizer.token()) {
            // D -> T K
            case STAR, VARIABLE -> {
                return new Node("D").add(T(), K());
            }
            default -> throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<'*', Variable>");
        }
    }

    private Node K() throws ParserException {
        try {
            switch (tokenizer.token()) {
                // K -> , D
                case COMMA -> {
                    tokenizer.nextToken(); // we “eat“ "," token
                    return new Node("K").add(new Node("\",\""), D());
                }
                // K -> eps
                case SEMICOLON -> {
                    return null;
                }
                default -> throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<',' | ';'>");
            }
        } catch (TokenizerException e) {
            throw new ParserException(e);
        }
    }

    private Node T() throws ParserException {
        Node res = new Node("T");
        try {
            switch (tokenizer.token()) {
                // T -> * T
                case STAR -> {
                    tokenizer.nextToken(); // we “eat“ "*" token
                    res.add(new Node("\"*\""), T());
                }
                // T -> Var
                case VARIABLE -> res.add(Var(), B());
                default -> throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<'*', Variable>");
            }
        } catch (TokenizerException e) {
            throw new ParserException(e);
        }
        return res;
    }

    private Node B() throws ParserException {
        try {
//            B -> '[' NUMB ']'
            if (tokenizer.token() == LPAR) {
                Node res = new Node("B");
                res.add(new Node("\"[\""));
                if(tokenizer.nextToken() == NUMB) {
                    res.add(new Node(tokenizer.tokenValue()));
                    if(tokenizer.nextToken() == RPAR) {
                        res.add(new Node("\"]\""));
                        tokenizer.nextToken(); // we “eat" ']' token
                        return res;
                    }
                }
            } else {
//                B -> eps;
                return null;
            }
        } catch (TokenizerException e) {
            throw new ParserException(e);
        }
        throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<'[' NUMB ']'>");
    }


    private Node Var() throws ParserException {
        try {
            if (tokenizer.token() == VARIABLE) {
                Node res = new Node("Var").add(new Node("\"" + tokenizer.tokenValue() + "\""));
                tokenizer.nextToken(); // we “eat“ Variable token
                return res;
            }
        } catch (TokenizerException e) {
            throw new ParserException(e);
        }
        throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<Variable>");
    }

    private Node Type() throws ParserException {
        try {
            if (tokenizer.token() == TYPE) {
                Node res = new Node("Type").add(new Node("\"" + tokenizer.tokenValue() + "\""));
                tokenizer.nextToken(); // we “eat“ Type token
                return res;
            }
        } catch (TokenizerException e) {
            throw new ParserException(e);
        }
        throw new ParserException(INCORRECT_TOKEN_EXPECTED + "<Type>");
    }
}
