%{
#include "y.tab.h"
#include <string.h>
%}

%option noyywrap

%%

"("          { return OP_OP; }
")"          { return OP_CP; }
"+"          { return OP_PLUS; }
"-"          { return OP_MINUS; }
"*"          { return OP_MULT; }
"/"          { return OP_DIV; }


"exit"       { return KW_EXIT; }
"and"        { return KW_AND; }
"or"         { return KW_OR; }
"not"        { return KW_NOT; }
"equal"      { return KW_EQUAL; }
"less"       { return KW_LESS; }
"nil"        { return KW_NIL; }
"list"       { return KW_LIST; }
"append"     { return KW_APPEND; }
"concat"     { return KW_CONCAT; }
"set"        { return KW_SET; }
"def"        { return KW_DEF; }
"for"        { return KW_FOR; }
"if"         { return KW_IF; }
"load"       { return KW_LOAD; }
"display"    { return KW_DISPLAY; }
"true"       { return KW_TRUE; }
"false"      { return KW_FALSE; }

";;"[^\n]*      { return COMMENT; }

[0-9]+b[0-9]+   { yylval.str_val = strdup(yytext); return VALUEF; }
[a-zA-Z][a-zA-Z0-9]* { yylval.str_val= strdup(yytext); return IDENTIFIER;}

[[:space:]]+    { /* ignore whitespace */ }

.           { printf("Unexpected character: %s\n", yytext); }

%%
