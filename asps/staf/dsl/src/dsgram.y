/* Copyright 1996, Lawrence Berkeley Laboratory */

/* dsgram.y - grammar for dataset definitions */
   
/*
modification history
--------------------
14jun96,whg  written
*/
/*
DESCRIPTION
yacc grammar for table types and dataset descriptors.
used to verify dataset spec and for implementation of parser
in c.
*/
%{
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
%}
%token CHAR
%token DATA
%token DOUBLE
%token FLOAT
%token IDENT
%token LONG
%token NUM
%token OCTET
%token SHORT
%token STRUCT
%token TYPE
%token UNSIGNED
%%
defs:
	/* nothing */
	| defs def
	;
def:
	dataset	{printf("dataset done\n");}
	| table_type	{printf("table_type done\n");}
	;
dataset:
	DATA data_def
	;
data_def:
	table_def
	| dataset_def 
	;
dataset_def:
	IDENT '{' data_item_list '}'
	;
data_item_list:
	data_item
	| data_item_list ',' data_item
	;
data_item:
	data_def
	| link_def
	;
link_def:
	'&' NUM
	;
table_def:
	IDENT '(' IDENT ',' NUM ')'
	;
table_type:
	TYPE struct_type
	;
struct_type:
	STRUCT IDENT '{' field_list '}'
	;
field_list:
	declaration
	| field_list declaration
	;
declaration:
	type_specifier declarator_list ';'
	;
declarator_list:
	declarator
	| declarator_list ',' declarator
	;
declarator:
	IDENT
	| declarator '[' NUM ']'
	;
type_specifier:
	base_type
	| IDENT
	| struct_type
	;
base_type:
	CHAR
	| OCTET
	| SHORT
	| UNSIGNED SHORT
	| LONG
	| UNSIGNED LONG
	| FLOAT
	| DOUBLE
	;
%%
/******************************************************************************
*
* function prototypes
*
*/
int yylex(void);
int yyparse(void);
/******************************************************************************
*
*/
void main()
{
	int c;

	for(;;) {
		printf("enter datasef or typedef\n");
		yyparse();
		while ((c =getchar()) > 0 && c != '\n');
	}
}
/******************************************************************************
*
*/
void yyerror(char *msg)
{
	fprintf(stderr, "%s\n", msg);
}
/******************************************************************************
*
*/
int yylex()
{
	int c, i, n;
	char buf[100];
	char *base[] = {"char", "data", "double", "float", "long", "octet",
		"short", "struct", "type", "unsigned"};
	int code[] = {CHAR, DATA, DOUBLE, FLOAT, LONG, OCTET,
		SHORT, STRUCT, TYPE, UNSIGNED};

	while (isspace(c = getchar()));
	if (c < 0) {
		exit(0);
	}
	if (!isalnum(c)) {
		return c;
	}
	if (isdigit(c)) {
		while (isdigit(c = getchar()));
		ungetc(c, stdin);
		return NUM;
	}
	for (i = 0; i < 99; i++) {
		buf[i] = c;
		if (!isalnum(c = getchar()) && c != '_')
			break;
	}
	ungetc(c, stdin);
	buf[i + 1] = '\0';
	for (i = 0, n = sizeof(base)/sizeof(base[0]); i < n; i++) {
		if (strcmp(base[i], buf) == 0)
			return code[i];
	}
	return IDENT;
}
