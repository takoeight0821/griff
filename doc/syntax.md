# Syntax

```bnf
lower : [a-z][a-zA-Z0-9_]*
upper : [A-Z][a-zA-Z0-9_]*

decs : (dec ';')*

dec : lower ':' type
    | lower lower* '=' exp
    | 'type' upper lower* '=' type
    
type : single_type '->' type
     | upper single_type*
     | single_type

single_type : 'Int'
            | 'Char'
            | 'String'
            | 'Bool'
            | upper single_type*
            | lower
            | '{' '}'
            | '{' lower ':' type (',' lower ':' type)* '}'
            | '<' lower ':' type ('|' lower ':' type)* '>'
            | '(' type ')'


exp : term 'as' type
    | binop
    | term
    | 'fn' lower lower* '->' exp
    | 'let' lower lower* '=' exp 'in' exp
    | 'let' 'rec' lower lower* '=' exp ('and' lower lower* '=' exp)* 'in' exp
    | 'case' exp 'of' ('|' pat '=>' exp)*
    | 'if' exp 'then' exp 'else' exp
    | '-' single_exp
    | single_exp

single_exp : literal
           | '{' lower '=' exp (',' lower '=' exp)* '}'
           | '<' lower '=' exp '>' 'as' type

literal : int
        | char
        | string
        | 'True'
        | 'False'
        | '{' '}'

binop : mul '+' binop
      | mul '-' binop
mul : equal '*' mul
    | equal '/' mul
equal : term '==' term
      | term '/=' term
      | term

pat : lower
    | '{' '}'
    | '{' lower '=' pat (',' lower '=' pat)* '}'
    | '<' lower '=' pat '>' 'as' type
```
