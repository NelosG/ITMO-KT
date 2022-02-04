grammar Grammar;

start : startRule grammarRule+ EOF;

startRule : '->' ParserIdentifier;

grammarRule
  : grammarParserRule
  | lexerRule
  ;

grammarParserRule : ParserIdentifier arguments? returnType? '->' productions ';';

arguments : '(' argument (',' argument)* ')';

argument : type argName;

argName : ParserIdentifier;

type : LexerIdentifier;

returnType: ':' type;

productions: production ('|' production)*;

production: extendedElements* returnExpression?;

extendedElements: element | CODE | MANYLINECODE;

returnExpression: '[' (ParserIdentifier | LexerIdentifier | CODE | MANYLINECODE) ']';

element: ParserIdentifier callAttributes? | LexerIdentifier;

callAttributes: '(' callAttribute (',' callAttribute)* ')';

callAttribute: ParserIdentifier | LexerIdentifier;

lexerRule
  : LexerIdentifier '->' terminals ';' # term
  | LexerIdentifier '=>' terminals ';' # skip
  ;

terminals
  : String
  | Regexp
  ;




ParserIdentifier : [a-z][a-zA-Z0-9$_]*;

LexerIdentifier : [A-Z][a-zA-Z0-9$_]*;

Regexp : '\'' (~('\'') | '\\\'')* '\'';

String: '"' (~["] | '\\"')* '"';

WS : [ \t\r\n]+ -> skip;

CODE : '{' ~[{}]+ '}';

MANYLINECODE : '`{' .+? '}`';
