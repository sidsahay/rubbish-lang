%{
#include <stdio.h>
#include "y.tab.h"
%}

stringliteral \"(\\.|[^"\\])*\"
integer [0-9]

%%

#topfunction return T_TOPFUNCTION;
#function return T_FUNCTION;
#endfunction return T_ENDFUNCTION;
push return T_PUSH;
pop return T_POP;
call return T_CALL;
ret return T_RET;
eq return T_EQ;
jmp return T_JMP;
jtrue return T_JTRUE;
add return T_ADD;
sub return T_SUB;

{stringliteral} {
	yytext[yyleng-1] = '\0';
	yylval.str_val = strdup(yytext+1);
	return T_STRING;
}

{integer} {
	yylval.int_val = atoi(yytext);
	return T_INTEGER;
}

%%
int yywrap() {return 1;}
