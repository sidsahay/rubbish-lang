%{
#include <stdio.h>
#include "lex.yy.c"

void yyerror(char* s);
%}

%token T_TOPFUNCTION
%token T_FUNCTION
%token T_ENDFUNCTION

%token T_STRING
%token T_INTEGER

%token T_PUSH
%token T_POP
%token T_CALL
%token T_RET
%token T_EQ
%token T_JMP
%token T_JTRUE
%token T_ADD
%token T_SUB

%start START

%union {
	int int_val;
	char* str_val;
};

%type <str_val> T_STRING
%type <int_val> T_INTEGER

%%

START: T_TOPFUNCTION T_STRING FUNCTION {printf("Program start, main function name: %s", $2);}
;

FUNCTION: T_FUNCTION T_STRING T_INTEGER FUNCTION_BODY T_ENDFUNCTION {printf("Found a function named %s\n", $2);}
;

FUNCTION_BODY: FUNCTION_BODY ZERO_INSTRUCTION | FUNCTION_BODY ONE_INSTRUCTION | ;

ZERO_INSTRUCTION: T_POP | T_ADD | T_SUB | T_RET | T_EQ {printf("Z\n");};

ONE_INSTRUCTION: T_PUSH T_STRING | T_PUSH T_INTEGER | T_CALL T_STRING | T_JMP T_INTEGER | T_JTRUE T_INTEGER {printf("O\n");};

%%

void yyerror(char* s) {printf("%s\n", s);}

	      
