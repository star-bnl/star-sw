l [A-Za-z]
lun [\_A-Za-z0-9]
%%
[<>]			{ RETURN2; }
\n			{ gLN++; strncpy(gL1,gL2,SZ-2); gL1[SZ-2]=0; *gL2=0;
			  RETURN2; }
\/\*			{ LS; gC=0; }
\*\/			{ LS; gC=7; DoComment(__LINE__,"\n"); }
\/\*.*\*\/		{ LS; DoOneLineComment(yytext); }
[\. \t]			{ LS; RETURN2; }
\/\/.*			{ LS; RETURN2; 
          DoComment(__LINE__,yytext); DoComment(__LINE__,"\n"); }
^\#include		{ LS;                    RETURN(INCLU); }
interface		{ LS;                    RETURN(INTER); }
STAFCV_T		{ LS; yylval.str=yytext; RETURN(STAFC); }
[\:\,\}\{\;\)\(]	{ LS;                    RETURN(SINGL); }
[\[\]]			{ LS;                    RETURN(SINGL); }
[0-9]+			{ LS; yylval.str=yytext; RETURN(NUMBE); }
\"[^\"]*\"		{ LS; yylval.str=yytext; RETURN(STRIN); }
"long"			{ LS; yylval.str=yytext; RETURN(CORBA); }
"short"			{ LS; yylval.str=yytext; RETURN(CORBA); }
"unsigned short"	{ LS; yylval.str=yytext; RETURN(CORBA); }
"unsigned long"		{ LS; yylval.str=yytext; RETURN(CORBA); }
"octet"			{ LS; yylval.str=yytext; RETURN(CORBA); }
"int"			{ LS; yylval.str=yytext; RETURN(CORBA); }
"float"			{ LS; yylval.str=yytext; RETURN(CORBA); }
"double"		{ LS; yylval.str=yytext; RETURN(CORBA); }
"char"			{ LS; yylval.str=yytext; RETURN(CORBA); }
"initialize"		{ LS; yylval.str=yytext; RETURN(ICALL); }
"call"			{ LS; yylval.str=yytext; RETURN(ICALL); }
"out"			{ LS; yylval.str=yytext; RETURN(INOUT); }
"inout"			{ LS; yylval.str=yytext; RETURN(INOUT); }
"in"			{ LS; yylval.str=yytext; RETURN(INOUT); }
"struct"		{ LS; yylval.str=yytext; RETURN(STRUC); }
{l}{lun}*		{ LS; yylval.str=yytext; RETURN(IDENT); }
{l}{lun}*\[[0-9]+\]	      { LS; yylval.str=yytext; RETURN(ARRAY); }
{l}{lun}*\[[0-9]+\]\[[0-9]+\] { LS; yylval.str=yytext; RETURN(ARRAY); }
{l}{lun}*\[[0-9]+\]\[[0-9]+\]\[[0-9]+\] { LS; yylval.str=yytext; RETURN(ARRAY); }
.	{ if(gC) { Fose(); fprintf(stderr,ERR_FORMAT2,gLN,yytext); exit(2); } }
