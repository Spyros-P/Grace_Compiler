%{
open Ast
%}

%token <int> INT
%token PLUS MINUS TIMES DIV LPAREN RPAREN EOF

%start main
%type <expr> main

%%

main:
  | expr EOF { $1 }

expr:
  | expr PLUS term { Add($1, $3) }
  | expr MINUS term { Sub($1, $3) }
  | term { $1 }

term:
  | term TIMES factor { Mul($1, $3) }
  | term DIV factor { Div($1, $3) }
  | factor { $1 }

factor:
  | INT { Int $1 }
  | LPAREN expr RPAREN { $2 }
