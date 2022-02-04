grammar JavaHighlight;

program : java EOF;

// keywords
FOR         : 'for';
WHILE       : 'while';
IF          : 'if';
ELSE        : 'else';

IMPORT      : 'import';
PACKAGE     : 'package';
EXTENDS     : 'extends';
IMPLEMENTS  : 'implements';

CLASS       : 'class';
INTERFACE   : 'interface';
SUPER       : 'super';

THIS        : 'this';
FINALLY     : 'finally';
SWITCH      : 'switch';
CASE        : 'case';
DEFAULT     : 'default';
TRY         : 'try';
CATCH       : 'catch';

RETURN      : 'return';
NEW         : 'new';
NULL        : 'null';
THROW       : 'throw';
THROWS      : 'throws';

// modifiers
PRIVATE      : 'private';
PUBLIC       : 'public';
PROTECTED    : 'protected';
STATIC       : 'static';
FINAL        : 'final';
VOLATILE     : 'volatile';
SYNCHRONIZED : 'synchronized';
ABSTRACT     : 'abstract';
TRANSIENT    : 'transient';

// types
VOID        : 'void';
INT         : 'int';
CHAR        : 'char';
DOUBLE      : 'double';
BYTE        : 'byte';
LONG        : 'long';
FLOAT       : 'float';
BOOLEAN     : 'boolean';

TRUE        : 'true';
FALSE        : 'false';


forHighlight
    : FOR
    | WHILE
    | IF
    | ELSE
    | PRIVATE
    | PUBLIC
    | PROTECTED
    | IMPORT
    | PACKAGE
    | EXTENDS
    | IMPLEMENTS
    | THIS
    | FINALLY
    | SWITCH
    | CASE
    | DEFAULT
    | TRY
    | CATCH
    | CLASS
    | INTERFACE
    | STATIC
    | SUPER
    | FINAL
    | RETURN
    | NEW
    | NULL
    | ABSTRACT
    | VOID
    | INT
    | CHAR
    | DOUBLE
    | BYTE
    | BOOLEAN
    | LONG
    | FLOAT
    | VOLATILE
    | TRANSIENT
    | SYNCHRONIZED
    | THROWS
    | TRUE
    | FALSE
    | THROW;


NOTCHAR : ~[a-zA-Z"\\\n\r\f\t <>&'@]+?;
NAME : [a-zA-Z][a-zA-Z0-9_]*;

SLASH : '\\';

CHARACTER : '\'' (~['] | '\\\'')+ '\'';


STRINGQ : '"';
SPECIAL : '<' | '>' | '&';
NEWLINE : '\r'? '\n' | '\r' | '\f';
TRIPLESTRINGQ : STRINGQ STRINGQ STRINGQ;
SPACE : ' ';
TAB   : '\t';

ANN : '@';

annotation : ANN (NAME | THROWS | RETURN);

stringOrCharacter
    :   CHARACTER
    |   STRINGQ STRINGQ
    |   STRINGQ
            (~(STRINGQ | SLASH | SPECIAL | NEWLINE | SPACE | TAB)
                | SLASH STRINGQ
                | SLASH ~(STRINGQ | SPECIAL | NEWLINE | SPACE | TAB)
                | special | tab | space
            )+
        STRINGQ
    |   TRIPLESTRINGQ newLine
            (~(TRIPLESTRINGQ | SLASH | SPECIAL | NEWLINE | SPACE | TAB)
                | SLASH STRINGQ
                | SLASH ~(STRINGQ | SPECIAL | NEWLINE | SPACE | TAB)
                | special | newLine | tab | space
            )+
        TRIPLESTRINGQ;



newLine  : NEWLINE;
space    : SPACE;
tab      : TAB;
special  : SPECIAL;
anyOther : NOTCHAR | NAME | SLASH | space | tab | newLine | special;
java     :  (forHighlight | stringOrCharacter | annotation | anyOther)+;
