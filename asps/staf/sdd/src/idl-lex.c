#include <stdio.h>
#include <stdlib.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# ifndef YYLMAX 
# define YYLMAX BUFSIZ
# endif 
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	int yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng;
#define YYISARRAY
char yytext[YYLMAX];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 4 "../src/idl.l"
		{ RETURN2; }
break;
case 2:

# line 5 "../src/idl.l"
		{ gLN++; strncpy(gL1,gL2,SZ-2); gL1[SZ-2]=0; *gL2=0;
			  RETURN2; }
break;
case 3:

# line 7 "../src/idl.l"
		{ LS; gC=0; }
break;
case 4:

# line 8 "../src/idl.l"
		{ LS; gC=7; DoComment(__LINE__,"\n"); }
break;
case 5:

# line 9 "../src/idl.l"
	{ LS; DoOneLineComment(yytext); }
break;
case 6:

# line 10 "../src/idl.l"
		{ LS; RETURN2; }
break;
case 7:

# line 11 "../src/idl.l"
		{ LS; RETURN2; 
          DoComment(__LINE__,yytext); DoComment(__LINE__,"\n"); }
break;
case 8:

# line 13 "../src/idl.l"
	{ LS;                    RETURN(INCLU); }
break;
case 9:

# line 14 "../src/idl.l"
	{ LS;                    RETURN(INTER); }
break;
case 10:

# line 15 "../src/idl.l"
	{ LS; yylval.str=yytext; RETURN(STAFC); }
break;
case 11:

# line 16 "../src/idl.l"
{ LS;                    RETURN(SINGL); }
break;
case 12:

# line 17 "../src/idl.l"
		{ LS;                    RETURN(SINGL); }
break;
case 13:

# line 18 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(NUMBE); }
break;
case 14:

# line 19 "../src/idl.l"
	{ LS; yylval.str=yytext; RETURN(STRIN); }
break;
case 15:

# line 20 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 16:

# line 21 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 17:

# line 22 "../src/idl.l"
{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 18:

# line 23 "../src/idl.l"
	{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 19:

# line 24 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 20:

# line 25 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 21:

# line 26 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 22:

# line 27 "../src/idl.l"
	{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 23:

# line 28 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(CORBA); }
break;
case 24:

# line 29 "../src/idl.l"
	{ LS; yylval.str=yytext; RETURN(ICALL); }
break;
case 25:

# line 30 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(ICALL); }
break;
case 26:

# line 31 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(INOUT); }
break;
case 27:

# line 32 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(INOUT); }
break;
case 28:

# line 33 "../src/idl.l"
		{ LS; yylval.str=yytext; RETURN(INOUT); }
break;
case 29:

# line 34 "../src/idl.l"
	{ LS; yylval.str=yytext; RETURN(STRUC); }
break;
case 30:

# line 35 "../src/idl.l"
	{ LS; yylval.str=yytext; RETURN(IDENT); }
break;
case 31:

# line 36 "../src/idl.l"
      { LS; yylval.str=yytext; RETURN(ARRAY); }
break;
case 32:

# line 37 "../src/idl.l"
{ LS; yylval.str=yytext; RETURN(ARRAY); }
break;
case 33:

# line 38 "../src/idl.l"
{ LS; yylval.str=yytext; RETURN(ARRAY); }
break;
case 34:

# line 39 "../src/idl.l"
{ if(gC) { Fose(); fprintf(stderr,ERR_FORMAT2,gLN,yytext); exit(2); } }
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

34,
0, 

6,
34,
0, 

2,
0, 

34,
0, 

11,
34,
0, 

34,
0, 

34,
0, 

13,
34,
0, 

1,
34,
0, 

30,
34,
0, 

30,
34,
0, 

12,
34,
0, 

30,
34,
0, 

30,
34,
0, 

30,
34,
0, 

30,
34,
0, 

30,
34,
0, 

30,
34,
0, 

30,
34,
0, 

30,
34,
0, 

34,
0, 

14,
0, 

4,
0, 

3,
0, 

7,
0, 

13,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

28,
30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

20,
30,
0, 

30,
0, 

30,
0, 

26,
30,
0, 

30,
0, 

30,
0, 

30,
0, 

5,
0, 

31,
0, 

30,
0, 

25,
30,
0, 

23,
30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

15,
30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

21,
30,
0, 

30,
0, 

27,
30,
0, 

30,
0, 

19,
30,
0, 

16,
30,
0, 

30,
0, 

30,
0, 

30,
0, 

22,
30,
0, 

30,
0, 

30,
0, 

29,
30,
0, 

30,
0, 

32,
0, 

30,
0, 

30,
0, 

30,
0, 

30,
0, 

10,
30,
0, 

30,
0, 

30,
0, 

30,
0, 

8,
0, 

30,
0, 

9,
30,
0, 

33,
0, 

24,
30,
0, 

18,
0, 

17,
0, 
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	6,24,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	6,24,	6,24,	45,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	46,0,	63,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,6,	
0,0,	0,0,	6,25,	0,0,	
0,0,	1,7,	0,0,	1,8,	
6,24,	0,0,	0,0,	45,46,	
1,9,	1,10,	2,23,	8,26,	
6,24,	9,27,	46,46,	63,46,	
109,114,	2,8,	9,28,	46,63,	
0,0,	1,11,	2,9,	0,0,	
6,24,	0,0,	1,12,	32,48,	
65,80,	6,24,	10,29,	10,29,	
10,29,	10,29,	10,29,	10,29,	
10,29,	10,29,	10,29,	10,29,	
48,65,	0,0,	0,0,	0,0,	
1,13,	13,32,	0,0,	80,92,	
0,0,	0,0,	0,0,	0,0,	
1,14,	64,79,	47,64,	6,24,	
1,3,	91,99,	2,13,	6,24,	
1,15,	1,16,	34,50,	1,17,	
15,33,	51,68,	1,18,	20,39,	
23,44,	1,19,	17,36,	15,34,	
1,20,	16,35,	2,15,	2,16,	
1,21,	2,17,	1,22,	18,37,	
2,18,	19,38,	22,43,	2,19,	
33,49,	20,40,	2,20,	35,51,	
36,52,	38,56,	2,21,	21,41,	
2,22,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	21,42,	
39,57,	40,58,	41,59,	42,60,	
43,61,	44,62,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,31,	49,66,	50,67,	52,69,	
12,30,	53,70,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
12,30,	12,30,	12,30,	12,30,	
27,45,	54,71,	55,72,	28,28,	
56,73,	57,74,	59,75,	60,76,	
27,45,	27,0,	61,77,	28,28,	
28,0,	31,47,	31,47,	31,47,	
31,47,	31,47,	31,47,	31,47,	
31,47,	31,47,	31,47,	37,53,	
62,78,	68,81,	69,82,	70,83,	
71,84,	37,54,	72,85,	74,86,	
75,87,	27,45,	37,55,	76,88,	
28,28,	77,89,	78,90,	27,45,	
81,93,	27,46,	28,28,	83,94,	
85,95,	88,96,	89,97,	27,45,	
90,98,	92,100,	28,28,	94,101,	
95,102,	97,103,	98,104,	99,105,	
100,106,	101,107,	102,108,	27,45,	
103,109,	104,110,	28,28,	107,112,	
27,45,	108,113,	111,115,	28,28,	
79,91,	79,91,	79,91,	79,91,	
79,91,	79,91,	79,91,	79,91,	
79,91,	79,91,	105,111,	105,111,	
105,111,	105,111,	105,111,	105,111,	
105,111,	105,111,	105,111,	105,111,	
112,116,	114,117,	27,45,	117,119,	
118,120,	28,28,	27,45,	119,121,	
114,118,	28,28,	120,122,	121,123,	
122,124,	124,125,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-15,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+-4,	0,		yyvstop+8,
yycrank+0,	0,		yyvstop+10,
yycrank+4,	0,		yyvstop+13,
yycrank+11,	0,		yyvstop+15,
yycrank+22,	0,		yyvstop+17,
yycrank+0,	0,		yyvstop+20,
yycrank+85,	0,		yyvstop+23,
yycrank+1,	yysvec+12,	yyvstop+26,
yycrank+0,	0,		yyvstop+29,
yycrank+7,	yysvec+12,	yyvstop+32,
yycrank+2,	yysvec+12,	yyvstop+35,
yycrank+2,	yysvec+12,	yyvstop+38,
yycrank+9,	yysvec+12,	yyvstop+41,
yycrank+10,	yysvec+12,	yyvstop+44,
yycrank+8,	yysvec+12,	yyvstop+47,
yycrank+27,	yysvec+12,	yyvstop+50,
yycrank+12,	yysvec+12,	yyvstop+53,
yycrank+3,	0,		yyvstop+56,
yycrank+0,	yysvec+6,	0,	
yycrank+0,	0,		yyvstop+58,
yycrank+0,	0,		yyvstop+60,
yycrank+-207,	0,		yyvstop+62,
yycrank+-210,	0,		yyvstop+64,
yycrank+0,	yysvec+10,	yyvstop+66,
yycrank+0,	yysvec+12,	yyvstop+68,
yycrank+173,	0,		0,	
yycrank+2,	yysvec+12,	yyvstop+70,
yycrank+16,	yysvec+12,	yyvstop+72,
yycrank+5,	yysvec+12,	yyvstop+74,
yycrank+10,	yysvec+12,	yyvstop+76,
yycrank+17,	yysvec+12,	yyvstop+78,
yycrank+126,	yysvec+12,	yyvstop+80,
yycrank+19,	yysvec+12,	yyvstop+83,
yycrank+28,	yysvec+12,	yyvstop+85,
yycrank+29,	yysvec+12,	yyvstop+87,
yycrank+35,	yysvec+12,	yyvstop+89,
yycrank+33,	yysvec+12,	yyvstop+91,
yycrank+33,	yysvec+12,	yyvstop+93,
yycrank+39,	0,		0,	
yycrank+-5,	yysvec+27,	0,	
yycrank+-12,	yysvec+27,	0,	
yycrank+1,	yysvec+31,	0,	
yycrank+10,	yysvec+12,	yyvstop+95,
yycrank+69,	yysvec+12,	yyvstop+97,
yycrank+64,	yysvec+12,	yyvstop+99,
yycrank+7,	yysvec+12,	yyvstop+101,
yycrank+82,	yysvec+12,	yyvstop+103,
yycrank+65,	yysvec+12,	yyvstop+105,
yycrank+92,	yysvec+12,	yyvstop+107,
yycrank+109,	yysvec+12,	yyvstop+109,
yycrank+109,	yysvec+12,	yyvstop+112,
yycrank+112,	yysvec+12,	yyvstop+114,
yycrank+0,	yysvec+12,	yyvstop+116,
yycrank+100,	yysvec+12,	yyvstop+119,
yycrank+98,	yysvec+12,	yyvstop+121,
yycrank+113,	yysvec+12,	yyvstop+123,
yycrank+133,	0,		0,	
yycrank+-13,	yysvec+27,	yyvstop+125,
yycrank+2,	0,		yyvstop+127,
yycrank+1,	yysvec+12,	yyvstop+129,
yycrank+0,	yysvec+12,	yyvstop+131,
yycrank+0,	yysvec+12,	yyvstop+134,
yycrank+125,	yysvec+12,	yyvstop+137,
yycrank+118,	yysvec+12,	yyvstop+139,
yycrank+130,	yysvec+12,	yyvstop+141,
yycrank+120,	yysvec+12,	yyvstop+143,
yycrank+124,	yysvec+12,	yyvstop+145,
yycrank+0,	yysvec+12,	yyvstop+147,
yycrank+123,	yysvec+12,	yyvstop+150,
yycrank+124,	yysvec+12,	yyvstop+152,
yycrank+144,	yysvec+12,	yyvstop+154,
yycrank+142,	yysvec+12,	yyvstop+156,
yycrank+138,	0,		0,	
yycrank+228,	0,		0,	
yycrank+1,	yysvec+12,	yyvstop+158,
yycrank+147,	yysvec+12,	yyvstop+160,
yycrank+0,	yysvec+12,	yyvstop+162,
yycrank+154,	yysvec+12,	yyvstop+165,
yycrank+0,	yysvec+12,	yyvstop+167,
yycrank+150,	yysvec+12,	yyvstop+170,
yycrank+0,	yysvec+12,	yyvstop+172,
yycrank+0,	yysvec+12,	yyvstop+175,
yycrank+137,	yysvec+12,	yyvstop+178,
yycrank+144,	yysvec+12,	yyvstop+180,
yycrank+139,	0,		0,	
yycrank+4,	yysvec+79,	0,	
yycrank+162,	yysvec+12,	yyvstop+182,
yycrank+0,	yysvec+12,	yyvstop+184,
yycrank+151,	yysvec+12,	yyvstop+187,
yycrank+163,	yysvec+12,	yyvstop+189,
yycrank+0,	yysvec+12,	yyvstop+191,
yycrank+160,	yysvec+12,	yyvstop+194,
yycrank+162,	0,		0,	
yycrank+172,	0,		yyvstop+196,
yycrank+180,	yysvec+12,	yyvstop+198,
yycrank+160,	yysvec+12,	yyvstop+200,
yycrank+167,	yysvec+12,	yyvstop+202,
yycrank+168,	yysvec+12,	yyvstop+204,
yycrank+168,	0,		0,	
yycrank+238,	0,		0,	
yycrank+0,	yysvec+12,	yyvstop+206,
yycrank+149,	yysvec+12,	yyvstop+209,
yycrank+172,	yysvec+12,	yyvstop+211,
yycrank+24,	yysvec+12,	yyvstop+213,
yycrank+0,	0,		yyvstop+215,
yycrank+181,	yysvec+105,	0,	
yycrank+195,	yysvec+12,	yyvstop+217,
yycrank+0,	yysvec+12,	yyvstop+219,
yycrank+189,	0,		0,	
yycrank+0,	0,		yyvstop+222,
yycrank+0,	yysvec+12,	yyvstop+224,
yycrank+188,	0,		0,	
yycrank+196,	0,		0,	
yycrank+193,	0,		0,	
yycrank+195,	0,		0,	
yycrank+204,	0,		0,	
yycrank+194,	0,		0,	
yycrank+0,	0,		yyvstop+227,
yycrank+193,	0,		0,	
yycrank+0,	0,		yyvstop+229,
0,	0,	0};
struct yywork *yytop = yycrank+309;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   1,   1,   1,   1,   1,   1, 
  1,   9,  10,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  9,   1,  34,   1,   1,   1,   1,   1, 
 40,  40,   1,   1,  40,   1,   9,   1, 
 48,  48,  48,  48,  48,  48,  48,  48, 
 48,  48,  40,  40,  60,   1,  60,   1, 
  1,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  91,   1,  91,   1,  95, 
  1,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  40,   1,  40,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.11	97/01/06 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
#ifdef YYISARRAY
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
#else
			if (yylastch >= &yytext[ yytextsz ]) {
				int	x = yylastch - yytext;

				yytextsz += YYTEXTSZINC;
				if (yytext == yy_tbuf) {
				    yytext = (char *) malloc(yytextsz);
				    memcpy(yytext, yy_tbuf, sizeof (yy_tbuf));
				}
				else
				    yytext = (char *) realloc(yytext, yytextsz);
				if (!yytext) {
				    fprintf(yyout,
					"Cannot realloc yytext\n");
				    exit(1);
				}
				yylastch = yytext + x;
			}
#endif
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}
