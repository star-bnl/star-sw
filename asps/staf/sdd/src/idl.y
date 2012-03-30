/* Yacc code for Staf IDL compiler, 
   begun October 5 1995 by Herb Ward.
** See bottom of file for mnemonicity tables (whatever those are).
*/
/******************************************************** yacc declarations */
%{
#include <stdio.h>
#include <sys/types.h>
#ifndef WIN32
#include <unistd.h>
#endif
  /* #if !defined(sun) && !defined(WIN32) 
#include <strings.h>
#else
#include <string.h>
#endif
  */
#include <string.h>

#include <stdlib.h>
#include "stic.h"
#define FORTRANCOMMENTSIZE 277
/* #define STUFF_FOR_64_BITS */
#define P printf(
#define PP printf(
#define EE fprintf(stderr,
#define RETURN if(!gC) DoComment(__LINE__,yytext); else return
#define RETURN2 if(!gC) DoComment(__LINE__,yytext)
#define F fprintf(stderr,
#define FF fprintf(ff,
#define FH fprintf(gFpH,
#define FINC fprintf(gFpInc,
#define SZ 170 /* line size in input file */
#define ISIZE 83 /* size of identifiers (eg interface names, prototype names */
#define PROTOTYPES 4 /* max prototypes in interface def */
#define INCDIR 37	/* max -I on command line */
#define INCDIRS 255	/* max size of path spec in a -I */
#define ERROR_FORMAT "stic: error on line %d:\n%s\n%s\n"
#define ERR_FORMAT2 "stic: error on line %d: unused character: %s\n"
#define OUTFILE 43
#define NOUTFILE 256
#define ARGS 90 /* max args per prototype */
#define INC 128
#define INIT  0
#define CALL 15
#define ERR Err(__LINE__)
#define IO_IN  0
#define IO_OUT 15
#define BLANK 30
#define IO_INOUT 3
#define EXE 70
#define INFILE  70   /* Max # of input files, including recursive includes */
#define INFILES 255  /*  Max size of any input, including path */
#define COL 300
#define COMMENTS 1200
#define OLC 82
#define SINGL (int)yytext[0]
#define TSIZE 50 /* big enuf for "unsigned short" */
#define CVSVERSION 100
#define MODULETAB 80 /* max number of in/out/inout table for a module */
#define INPUTTAB 80 /* max number of included tables for a module idl file */
#define MODULETABSIZE 40
#define INPUTTABSIZE 40
int gHaveIncludedPamIdl, gHaveSeen_STAFCV_T, gHaveSeen_amiModule;
int gFtc,gJj,gIi,gLN=1,gNPamNames=0,gNArgName[PROTOTYPES],gNProto=0;
int gC=7,gNColTypes=0,gNColNames=0,gNIncFile=0,gNTblName=0;

int gNmoduleTable,gNinputTable;
char gModuleTable[MODULETAB][MODULETABSIZE+1];
char gInputTable[INPUTTAB][INPUTTABSIZE+1];

char gIncDir[INCDIR][INCDIRS];
char gOrigInputFile[81];
int gNincDir,gNOutFile=0,gNoMoreComments=0;
int  gOptions,gOptionH,gOptiont,gOptioni,gOptionM,gOptionT,gOptionstatic,gOptiondynamic;
int  gOptionr,gOptionf;
/* An option to process files _quietly_ */
int  gOptionq;
char gOlc[COL][OLC],gOutFile[NOUTFILE][OUTFILE+2];
char gInFile[INFILE][INFILES+1];
int gNInFile=0;
char gPass[100],gComments[COMMENTS];
char gOptioniTempFile[40],gExeName[EXE+2];
char gInFileName[INFILES+2];
char gInFileNameNoPath[INFILES+2];
char gColName[COL][ISIZE+2];
char gTable[ISIZE+2];
char gIo[PROTOTYPES][ARGS];
char gIncFile[INC][ISIZE+2];
char gL2[SZ+2],gL1[SZ+2];
char gPam[ISIZE+2],gMkCc;
char gPamUp[ISIZE+2];
char gPn[PROTOTYPES][ISIZE+2];
char gArgName[PROTOTYPES][ARGS][ISIZE+2];
char gColType[COL][TSIZE+2];
char gDataType[PROTOTYPES][ARGS][TSIZE+2];
char *gCvsVersionRaw="$Id: idl.y,v 1.31 2012/03/30 00:30:58 jeromel Exp $";
char gCvsVersion[CVSVERSION+1];
char gFncType[PROTOTYPES][TSIZE+2];
char VERSION[132];
FILE *gFpH,*gFpInc,*gFile;
FILE *yyin; /* declaration for yyin made by lex is removed in Makefile */
/*********************************************************** prototypes */
void TemplateFBegin(char *fn,FILE *ff);
void Ose(void);
void Pam(void);
void TemplateFMiddle(FILE *ff);
void TemplateFEnd(FILE *ff);
void TemplateCBegin(FILE *ff);
void TemplateCMiddle(char *pamname,FILE *ff);
void TemplateCEnd(FILE *ff);
void Init(void);
void OpenAllTblOutput(void);
void HandleOneInputFile(char *inFile);
void Tbl(void);
void Usage(void);
%}
%union { char *str; }
%start finish
%token INTER CORBA ICALL STRUC
%token PROTO IDENT NUMBE INOUT STRIN INCLU ARRAY STAFC
%% /***************************************************************** rules */
finish	: theFile { } | error { Error(); }				  ;
theFile : | theFile onePart						  ;
onePart : pam { Pam(); Init(); } | tbl { Tbl(); Init(); } 		  ;
tbl	: STRUC tblName '{' tAndNs '}' ';'				  ;
tblName	: IDENT { TblName(yylval.str); }				  ;
tAndNs	: | tAndNs tAndN						  ;
tAndN	: colType cols 							  ;
colType : CORBA { gFtc=gNColNames; ColType(yylval.str); }                 ;
cols	: | cols col							  ;
col	: IDENT { Col(yylval.str); } cs | ARRAY { Col(yylval.str); } cs   ;
cs	: ';' | ','							  ;
pam	: incs INTER pamName ':' IDENT '{' protos '}' ';' { gMkCc=7; }    ;
pam	: incs INTER pamName           '{' protos '}' ';' { gMkCc=0; }    ;
pamName	: IDENT { PamName(yylval.str); }				  ;
incs	: | incs inc							  ;
inc	: INCLU incFile							  ;
incFile	: STRIN { IncludeFileName(yylval.str); }			  ;
protos	: | protos proto						  ;
proto	: fncType fncName '(' args ')' ';'				  ;
fncType	: STAFC { FncType(yylval.str); }				  ;
fncName	: ICALL { PrototypeName(yylval.str); }				  ;
args	: | args arg | args arg ','					  ;
arg	: inOut argType argName						  ;
inOut	: INOUT { InOut(yylval.str); }					  ;
argName	: IDENT { ArgName(yylval.str); }				  ;
argType	: IDENT { ArgType(yylval.str); } | CORBA { ArgType(yylval.str); } ;
%% /************************************************************* functions */
void ToUpper(char *out,const char *in);
char *StrippedInFileName(int uppercase) {
  char *cc,tmp[100];
  static char rv[123];
  strcpy(rv,gInFileNameNoPath);
  cc=strstr(rv,".idl"); if(cc) cc[0]=0;
  if(uppercase) { ToUpper(tmp,rv); strcpy(rv,tmp); }
  return rv;
}
void DoComment(int codeLineNum,char *xx) {
  char *cc;
  static int len=-10;
  char *rr="\nCOMMENTS TRUNCATED";
  if(len<0) len=strlen(rr)+5;
  if(xx[0]=='/'&&xx[1]=='/') cc=xx+2; else cc=xx;
  if(gNoMoreComments) return;
  if(strlen(cc)+strlen(gComments)>COMMENTS-len) {
    strcat(gComments,rr); gNoMoreComments=7; return;
  }
  strcat(gComments,cc);
}
void Fose(void) {
  fprintf(stderr,"ooooooooooooooooooooooooooooooooooooooooooooooooooooooo\n");
}
void Idl2Fortran(char *out,const char *in) {
       if(!strcmp(in,"int"))   strcpy(out,"INTEGER*4");
  else if(!strcmp(in,"unsigned int"))   strcpy(out,"INTEGER*4");
  else if(!strcmp(in,"long"))   strcpy(out,"INTEGER*4");
  else if(!strcmp(in,"unsigned long"))   strcpy(out,"INTEGER*4");
  else if(!strcmp(in,"short"))  strcpy(out,"INTEGER*2");
  else if(!strcmp(in,"unsigned short"))  strcpy(out,"INTEGER*2");
  else if(!strcmp(in,"float"))  strcpy(out,"REAL*4");
  else if(!strcmp(in,"double")) strcpy(out,"REAL*8");
  else if(!strcmp(in,"octet")) strcpy(out,"LOGICAL*1");
  else if(!strcmp(in,"char")) strcpy(out,"CHARACTER*");
  else strcpy(out,in);
}
void ToLower(char *out,const char *in) {
  int off,ii; char cc;
  for(ii=strlen(in);ii>=0;ii--) {
    cc=in[ii]; if(cc>='A'&&cc<='Z') off='m'-'M'; else off=0; out[ii]=cc+off;
  }
}
void ToUpper(char *out,const char *in) {
  int off,ii; char cc;
  for(ii=strlen(in);ii>=0;ii--) {
    cc=in[ii]; if(cc>='a'&&cc<='z') off='A'-'a'; else off=0; out[ii]=cc+off;
  }
}
void OutputCommentsFromIdlFile(int fortranOrC,FILE *ff) {
  char *cc,buf[COMMENTS];
  if(*gComments==0) return;
  if(fortranOrC==1) {                                           /* FORTRAN */
    FF"C COMMENTS FROM IDL FILE:\n"); strcpy(buf,gComments);
    cc=strtok(buf,"\n");
    while(cc) {
      FF"C %s\n",cc); 
      cc=strtok(NULL,"\n");
    }
  }
  if(fortranOrC==2) {                                                 /* C */
    FF"/* COMMENTS FROM IDL FILE:\n");
    FF"%s ",gComments);
    FF"*/\n");
  }
}
void StandardBlurb(int fortranOrC,char *mode,FILE *ff) {
  if(strcmp(mode,"w")) return;
  switch(fortranOrC) {
    case 0: break;
    case 1: FF"C %s.inc\n",gTable); break;
    case 2: FF"/* %s.h */\n",gTable); break;
    default: Err(__LINE__);
  }
  if(fortranOrC==1) {
    FF"C This file was made by the idl compiler \"stic\". Do not edit.\n");
    FF"C This was generated for version '%s'\n",(VERSION[0]=='\0'?"(unspecified)":VERSION));
    FF"C Instead, edit the source idl file and then re-run the compiler.\n");
    FF"C For help, type contact Craig Tull or Herb Ward.\n"); 
  } else if(fortranOrC==2) {
    FF"/* This file was made by the idl compiler \"stic\". Do not edit.\n"); 
    FF"** This was generated for version '%s'\n",(VERSION[0]=='\0'?"(unspecified)":VERSION));
    FF"** Instead, edit the source idl file and then re-run the compiler.\n");
    FF"** For help, type contact Craig Tull or Herb Ward. */\n");
  } else ERR;
  OutputCommentsFromIdlFile(fortranOrC,ff);
}
ColType(char *xx) {
  if(gNColTypes>=COL) {
    F"You are specifying too many columns in the idl file.  Max=%d.\n",COL);
    exit(2);
  }
  if(!strcmp(xx,"int")) {
    EE"Fatal error: \"int\" is not a valid IDL data type.\n"); exit(2);
  }
  strncpy(gColType[gNColTypes],xx,TSIZE); /* big enuf for "unsigned short" */
  gNColTypes++;
}
Col(char *xx) {
  if(gNColNames>=COL) {
    F"You are specifying too many columns in the idl file.  Max=%d.\n",COL);
    exit(2);
  }
  strncpy(gColName[gNColNames],xx,ISIZE);
  gNColNames++;
  if(gNColTypes<gNColNames) {
    strcpy(gColType[gNColTypes],gColType[gNColTypes-1]); gNColTypes++;
  }
  if(gNColNames!=gNColTypes) Err(__LINE__);
}
TblName(char *xx) {
  if(strlen(xx)>INPUTTABSIZE) ERR;
  if(gNinputTable>=INPUTTAB) ERR;
  strcpy(gInputTable[gNinputTable++],xx);
  if(++gNTblName>1) { F"Only one table per idl file.\n"); exit(2); }
  strncpy(gTable,xx,ISIZE);
}
Err(int xx) {
  F"ooooooooooooooooooooooooooooooooooooooooooooooooooooo\n");
  F">>>>> STIC FATAL ERROR line %d of %s.  ward@physics.utexas.edu.\n",xx,
  __FILE__);
  exit(2);
}
void IdlToCOrCpp(char *out,char *in) {
#ifdef STUFF_FOR_64_BITS
       if(!strcmp(in,"long"))            strcpy(out,"IDL_LONG");
  else if(!strcmp(in,"unsigned long"))   strcpy(out,"IDL_ULONG");
  else if(!strcmp(in,"short"))           strcpy(out,"IDL_SHORT");
  else if(!strcmp(in,"unsigned short"))  strcpy(out,"IDL_USHORT");
  else if(!strcmp(in,"float"))           strcpy(out,"IDL_FLOAT");
  else if(!strcmp(in,"double"))          strcpy(out,"IDL_DOUBLE");
  else if(!strcmp(in,"octet"))           strcpy(out,"IDL_OCTET");
  else if(!strcmp(in,"char"))            strcpy(out,"IDL_CHAR");
#else
       if(!strcmp(in,"octet"))         strcpy(out,"unsigned char");
  else if(!strcmp(in,"long"))          strcpy(out,"int"); 
  else if(!strcmp(in,"unsigned long")) strcpy(out,"unsigned int"); 
  else strcpy(out,in);
#endif
}
char *Up(const char *x) {
  ToUpper(gPass,x); return gPass;
}
void FirstFile(FILE *ff) {
  FF"#ifndef STAF_St_%s_Table\n",gTable);
  FF"#define STAF_St_%s_Table\n",gTable);
  FF"\n");
  FF"#include \"St_Table.h\"\n");
  FF"#include \"%s.h\"\n",gTable); 
  FF"\n");
  FF"class St_%s : public St_Table\n",gTable);
  FF"{\n");
  FF"public:\n");
  FF" St_%s() : St_Table(\"%s\",sizeof(%s_st)) {SetType(\"%s\");}\n",gTable,gTable,gTable,gTable);
  FF" St_%s(Text_t *name,const Char_t *type=\"%s\") : St_Table(name,sizeof(%s_st)) {SetType(type);}\n",gTable,gTable,gTable);
  FF" St_%s(Int_t n): St_Table(\"%s\",n,sizeof(%s_st)) {SetType(\"%s\");}\n",gTable,gTable,gTable,gTable);
  FF" St_%s(Text_t *name,Int_t n): St_Table(name,n,sizeof(%s_st)) {SetType(\"%s\");}\n",gTable,gTable,gTable);
  FF" %s_st *GetTable(){ return (%s_st *)s_Table;}\n",gTable,gTable);
  FF"\n");
  FF" ClassDef(St_%s,0) // class to wrap the \"%s\" STAF table\n",
      gTable,gTable);
  FF"};\n");
  FF"\n");
  FF"#endif\n");
}
void SecondFile(FILE *ff) {
  FF"#include \"tables/St_%s_Table.h\"\n",gTable);
  FF"#include \"Stypes.h\"\n");
  FF"TableImp(%s)\n",gTable);
  FF"void St_%s::Streamer(TBuffer &b){St_Table::Streamer(b);}\n",gTable);
}
void WriteTwoRootTableFiles(void) {
  FILE *ff; char fn[123];

  sprintf(fn,"St_%s_Table.h",gTable);
  ff=fopen(fn,"w"); if(!ff) ERR; FirstFile(ff); fclose(ff);

  sprintf(fn,"St_%s_Table.cxx",gTable);
  ff=fopen(fn,"w"); if(!ff) ERR; SecondFile(ff); fclose(ff);

}
void DotHFileTbl(void) {
  char colType[111]; int ii;
  if(gOptionM || gOptionT) Err(__LINE__);
  if(gOptionr) WriteTwoRootTableFiles();
  FH"#ifndef %s_H\n",Up(gTable));
  FH"#define %s_H\n",Up(gTable));
  /* 960529a FH"#include \"table_header.h\"\n"); */
  FH"#define %s_SPEC \\\n",Up(gTable));
  FH"\"struct %s { \\\n",gTable);
  for(ii=0;ii<gNColNames;ii++) {
    FH"\t%s %s; \\\n",gColType[ii],gColName[ii]);
  }
  FH"};\"\n");
  FH"typedef struct %s_st {\n",gTable);
  for(ii=0;ii<gNColNames;ii++) {
    IdlToCOrCpp(colType,gColType[ii]);
    FH"\t%s %s; %s\n",colType,gColName[ii],gOlc[ii]);
  }
  FH"} %s_ST;\n",Up(gTable));
  FH"#endif /* %s_H */\n",Up(gTable));
}
void ReverseIndices(char *x) { /* FORTRAN indices are backwards */
  int dims[15];
  int ii, jj;
  char *a, *sx, buf[30];

  sx = x;
  for (ii = 0; ii < 15; ii++) {
    a = strstr(x, "("); 
    if (!a) break;
    dims[ii] = atoi(a+1);
    x = strstr(a, ")");
  }
  a = strstr(sx, "(");
  if( !a ) return;
  a[0] = 0;
  strcat(sx,"(");
  for (jj = ii; jj > 0; jj--) {
    sprintf(buf, " %d ", dims[jj-1]); strcat(sx, buf);
    if(jj>1) strcat(sx,", ");
  }
  strcat(sx,")");
}
void BracksToParens(char *out,char *xx) {
  int ii;
  strcpy(out,xx);
  for(ii=strlen(out)-1;ii>=0;ii--) {
    if(out[ii]=='[') out[ii]='(';
    if(out[ii]==']') out[ii]=')';
  }
  ReverseIndices(out);
}
void ChangeCommentFromCToFortran(char *out,char *in) {
  char *cc;
  if(strlen(in)>FORTRANCOMMENTSIZE) ERR;
  strcpy(out,in);
  cc=strstr(out,"/*");
  while(cc) { cc[0]='!'; cc[1]=' '; cc=strstr(out,"/*"); }
  cc=strstr(out,"*/");
  while(cc) { cc[0]='0'; cc[1]=' '; cc=strstr(out,"*/"); }
}
void DotIncFileTbl(void) {
  char buf2[111],*cc,buf[111],fort_ran[77],uppercase[111],blank[BLANK];
  char fortranComment[FORTRANCOMMENTSIZE+1];
  int col,ii,nn,totLen,here;
  ToUpper(uppercase,gTable);
  FINC"#ifndef %s_INC\n",uppercase);
  FINC"#define %s_INC\n",uppercase);
  FINC"#endif\n");
  /* 960529b  FINC"#include \"table_header.inc\"\n"); */
  FINC"\tSTRUCTURE /%s_ST/\n",uppercase);
  for(col=0;col<gNColNames;col++) {
    Idl2Fortran(fort_ran,gColType[col]);
    if(strcmp(gColType[col],"char")) {
      BracksToParens(buf,gColName[col]);
    } else {
      strcpy(buf,gColName[col]); cc=strstr(buf,"[");
      if(cc) {
        if(!strstr(cc,"]")) { F"Crummy stuff: %s.\n",gColName[col]); exit(2); }
        nn=atoi(cc+1); cc[0]=0; sprintf(buf2,"%s%d",fort_ran,nn);
      } else sprintf(buf2,"%s1",fort_ran);
      strcpy(fort_ran,buf2);
    }
    ChangeCommentFromCToFortran(fortranComment,gOlc[col]);
    FINC"\t%9s %s %s\n",fort_ran,buf,fortranComment);
  }
  FINC"\tEND STRUCTURE\n");

  /* new as of 960429 ----------------------------*/
  FINC"C\n");
  FINC"      CHARACTER*(*) %s_SPEC\n",Up(gTable));
  FINC"      PARAMETER    (%s_SPEC=\n",Up(gTable));
  for(ii=BLANK-1;ii>=0;ii--) blank[ii]=' '; blank[BLANK-1]=0;
  totLen=strlen(gTable)+7;
  here=BLANK-totLen; if(here>=0&&here<BLANK) blank[here]=0; else blank[0]=0;
  FINC"     + ' struct %s {' %s //\n",gTable,blank);
  for(col=0;col<gNColNames;col++) {
    for(ii=BLANK-1;ii>=0;ii--) blank[ii]=' '; blank[BLANK-1]=0;
    totLen=strlen(gColType[col])+strlen(gColName[col]);
    here=BLANK-totLen; if(here>=0&&here<BLANK) blank[here]=0; else blank[0]=0;
    FINC"     + ' %s %s;' %s //\n",gColType[col],gColName[col],blank);
  }
  FINC"     + ' };')\n");
  FINC"C\n");
}
void Tbl(void) {
/*
  if(gOptiont) {
    Ose(); PP"You have used option -t with a table-type idl file.\n");
    PP"This does not make sense.  The option -t means 'templates\n");
    PP"only', and there is no such thing as a template derived from a\n");
    PP"table-type idl file.\n");
    PP"FATAL ERROR   FATAL ERROR   FATAL ERROR   FATAL ERROR   \n");
    exit(2);
  }
  */
  OpenAllTblOutput();
  if(gOptionM || gOptionT) return;
  DotHFileTbl(); 
  DotIncFileTbl();
}
DumpGlobalsPam(void) {
  int ii,jj; char buf[5];
  P"The name of the PAM is %s, %d include files, %d prototypes.\n","xyz",
  gNIncFile,gNProto);
  for(ii=0;ii<gNIncFile;ii++) {
    P"Include file %d: %s.\n",ii+1,gIncFile[ii]);
  }
  for(ii=0;ii<gNProto;ii++) {
    P"Prototype %d: fncName=%s, %d args.\n",ii+1,gPn[ii],gNArgName[ii]);
    P"  Args:\n");
    for(jj=0;jj<gNArgName[ii];jj++) {
      if(gIo[ii][jj]==IO_IN) strcpy(buf,"in");
      else if(gIo[ii][jj]==IO_OUT) strcpy(buf,"out");
      else if(gIo[ii][jj]==IO_INOUT) strcpy(buf,"inout");
      else Err(__LINE__);
      P"    %s %s %s\n",buf,gDataType[ii][jj],gArgName[ii][jj]);
    }
  }
  exit(2);
}
void ModeFromFn(char *fn,char *mode) {
  int ii;
  for(ii=gNOutFile-1;ii>=0;ii--) { if(!strcmp(fn,gOutFile[ii])) break; }
  if(ii<0) {
    if(gNOutFile>=NOUTFILE) Err(__LINE__);
    strncpy(gOutFile[gNOutFile++],fn,OUTFILE); strcpy(mode,"w");
  } else {
    strcpy(mode,"a");
  }
}
void ExtendOutList(char *fn) {
  char junk[12];
  ModeFromFn(fn,junk);
}
void StripOffIdl(char *in,char *out) {
  char *cc,buf[123],*xx; int ii;
  strcpy(buf,in);
  for(ii=strlen(buf);ii>=0;ii--) if(buf[ii]=='/') break; xx=buf+ii+1;
  cc=strstr(xx,".idl"); if(cc==NULL) Usage(); cc[0]=0; strcpy(out,xx);
}
FILE *OpenOnePamOutput(char *x) {
  char mode[3],fn[111],buf[111]; int fortranOrC;
  if(strstr(x,".h")) fortranOrC=2;
  else if(strstr(x,".inc")) fortranOrC=1; else ERR;
  StripOffIdl(gInFileNameNoPath,buf);
  sprintf(fn,"%s%s",buf,x);
  ModeFromFn(fn,mode);
  if(gOptionM || gOptionT) return NULL;
  gFile=fopen(fn,mode); /* gOptionf OK */
  if(!strcmp(mode,"w") && !gOptionq) PP"  out: %s\n",fn);
  StandardBlurb(fortranOrC,mode,gFile);
  if(gFile==NULL) { F"Can't write %s.\n",fn); exit(2); }
  return gFile;
}
void PrintTheArgs(FILE *ff,int ii) {
  int jj; char ast[2],comma[12],headerName[111],tableType[111],up[111];
  for(jj=0;jj<gNArgName[ii];jj++) {
    if(gIo[ii][jj]==IO_IN) *ast=0;
    else if(gIo[ii][jj]==IO_OUT) strcpy(ast,"*");
    else if(gIo[ii][jj]==IO_INOUT) strcpy(ast,"*");
    else Err(__LINE__);
    if(jj==gNArgName[ii]-1) strcpy(comma,""); else strcpy(comma,",");
    sprintf(headerName,"*%s_h",gArgName[ii][jj]);
    ToUpper(up,gDataType[ii][jj]);
    sprintf(tableType,"%s_ST",up);
    FF"  TABLE_HEAD_ST  %16s, %16s    *%s_d %s\n",
    headerName,tableType,gArgName[ii][jj],comma);
  }
  FF");\n");
}
void Eose(void) {
  EE"----------------------------------------------------------------\n");
}
void Ose(void) {
  if(gOptionM || gOptionT) return;
  PP"----------------------------------------------------------------\n");
}
void PamOutputDotHFile(void) {
  char dt[22],ast[2],comma[12],fName[60],headerName[111];
  char didPam=0,*cc,tmp2[99],tableType[111],tmp[99];
  int ii,jj,initOrCall;
  FILE *ff;
  /* Q14 */
  ff=OpenOnePamOutput(".h"); if(gOptionM || gOptionT) return;
  FF"/* %s.h */\n",StrippedInFileName(0));
  FF"#ifndef %s_H\n",StrippedInFileName(7));
  FF"#define %s_H\n",StrippedInFileName(7));
  FF"/*----------------------------------------------- INCLUDES   --*/\n");
  for(ii=0;ii<gNIncFile;ii++){
    strcpy(tmp,gIncFile[ii]);
    cc=strstr(tmp,".idl"); if(cc!=NULL) *cc=0; /* don't del, modifies tmp */
    if(!strcmp(tmp,"\"asu")) ToUpper(tmp2,tmp); else strcpy(tmp2,tmp);
    if(strcmp(tmp2,"\"ASU")) {
      FF"#include %s.h\"\n",tmp2);
      /* BBB PP"tmp2=%s.\n",tmp2); Sleep(1); */
      if(!strcmp(tmp2,"\"PAM")) didPam=7;
    } else {
      P"\n\n"); Ose(); 
      P"%cWARNING: Please ",7);
      P"remove '#include ASU.idl' from the input file.\n\n"); sleep(6);
    }
  }
  if(!didPam) FF"#include \"%s.h\"\n","PAM"); /* 960529c */
  FF"/*----------------------------------------------- MACROS     --*/\n");
  if(gNProto!=1) Err(__LINE__);
  if(gNArgName[gNProto-1]<0) Err(__LINE__);
  FF"#define %s_RANK %d\n",gPamUp,gNArgName[gNProto-1]);
  FF"/*----------------------------------------------- FORTRAN NAMES  --*/\n");
  FF"#ifdef F77_NAME\n#define %s_ F77_NAME(%s,%s)\n#endif\n",gPam,gPam,gPamUp);
  FF"#ifndef type_of_call\n#define type_of_call\n#endif\n");
  FF"/*----------------------------------------------- TYPEDEFS   --*/\n");
  FF"typedef STAFCV_T (type_of_call *%s_FT)\n(\n",gPamUp);
  if(gNProto!=1) Err(__LINE__); /* PrintTheArgs needs protection from arg2=0 
    if gNProto!=1. */
  PrintTheArgs(ff,0);	/* comment 917a, if gNProto!=1, may need arg2!=0 */
  for(ii=0;ii<gNProto;ii++) { /* gNProto always = 1 (2??) */
    if(!strcmp(gPn[ii],"initialize")) {
      sprintf(fName,"%s_initialize",gPam); initOrCall=INIT;
    } else if(!strcmp(gPn[ii],"call")) {
      sprintf(fName,"%s_",gPam); initOrCall=CALL;
    } else {
      Err(__LINE__);
    }
    FF"/*----------------------------------------------- PROTOTYPES --*/\n");
    FF"extern CC_P STAFCV_T type_of_call %s (\n",fName); /* pam.h */
    if(initOrCall==INIT&&0) { /* 960215 disabled, not in CET's examples */
      for(jj=0;jj<gNArgName[ii];jj++) {
        if(gIo[ii][jj]==IO_IN) *ast=0;
        else if(gIo[ii][jj]==IO_OUT) strcpy(ast,"*");
        else if(gIo[ii][jj]==IO_INOUT) strcpy(ast,"*");
        else Err(__LINE__);
        if(jj==gNArgName[ii]-1) strcpy(comma,"\n);"); else strcpy(comma,",");
        if(!strcmp(gDataType[ii][jj],"string")) {
          strcpy(dt,"char"); strcpy(ast,"*");
        } else strcpy(dt,gDataType[ii][jj]);
        FF"  %15s %1s %s %s\n",dt,ast,gArgName[ii][jj],comma);
      }
    } else if(initOrCall==CALL) {
      PrintTheArgs(ff,ii);
    } else Err(__LINE__);
  }
  FF"#ifdef __cplusplus\n");
  /* Q01 */
/*
  FF"extern CC_P const char * %s_version();\n",gPam);
*/
  FF"extern CC_P STAFCV_T %s_load_ami(amiBroker *broker);\n",gPam); /* pam.h */
  FF"#endif /* __cplusplus */\n");
  FF"#endif /* %s_H */\n",StrippedInFileName(7));
  fclose(ff);
}
void StarFortranComment(int initOrCall,FILE *ff) {
  int type,lookfor,jj; char *label,headerName[111];
  if(initOrCall==CALL) {
    for(type=0;type<3;type++) {
      switch(type) {
        case 0: lookfor=IO_IN; label="IN"; break;
        case 1: lookfor=IO_INOUT; label="INOUT"; break;
        case 2: lookfor=IO_OUT; label="OUT"; break;
      }
      FF"CC:       %5s:\n",label);
      for(jj=0;jj<gNArgName[0];jj++) {
        if(gIo[0][jj]!=lookfor) continue;
        sprintf(headerName,"%s_h",gArgName[0][jj]);
        FF"CC:  %15s     - description here\n",gArgName[0][jj]);
        FF"CC:  %15s     - Header Structure for %s\n",headerName,
        gArgName[0][jj]);
      }
    }
  }
}
void PamTemplateFortran(void) {
  char headerName[111],tableType[111],fn[100]; FILE *ff; int ii,jj,initOrCall;
  int continuation; char comma[17],fName[111];
  /* Q02 */
  sprintf(fn,"%s.F.template",StrippedInFileName(0));
  if(gOptionM || gOptionT) { ExtendOutList(fn); return; }
  if(gOptionf) return; ff=fopen(fn,"w"); /* PamTemplateFortran() */
  if(ff==NULL) { F"Can't write %s.\n",fn); exit(2); }

  TemplateFBegin(fn,ff);
  FF"      INTEGER*4 FUNCTION %s(\n",gPamUp);
  for(ii=0;ii<gNProto;ii++) {
    if(!strcmp(gPn[ii],"initialize")) initOrCall=INIT;
    else if(!strcmp(gPn[ii],"call")) initOrCall=CALL;
    else Err(__LINE__);
    if(initOrCall==CALL) {
      for(jj=0;jj<gNArgName[ii];jj++) {
        if(jj==gNArgName[ii]-1) strcpy(comma,") "); else strcpy(comma,",");
        sprintf(headerName,"%s_h",gArgName[ii][jj]);
        continuation=jj+1; if(continuation>9) continuation=9;
        FF"     %d   %17s, %17s %s\n",
        continuation,headerName,gArgName[ii][jj],comma);
      }
    }
  }
  FF"      IMPLICIT NONE\n");
  /* Q03 */
  FF"#include \"%s.inc\"\n",StrippedInFileName(0));
  TemplateFMiddle(ff);
  if(gNProto!=1) ERR; StarFortranComment(initOrCall,ff);
  TemplateFEnd(ff);
  fclose(ff); 
  if (!gOptionq) 
     F"  out: %s\n",fn);
}
void PamCC(void) {
  char headerName[111],tableType[111],fn[100]; FILE *ff; int ii,jj,initOrCall;
  char sh[89],st[89],comma[17],fName[111],withAsterisk[79];
  if(!gMkCc) return;
  /* Q04 */
  sprintf(fn,"%s_i.cc",StrippedInFileName(0));
  if(gOptionM || gOptionT) { ExtendOutList(fn); return; }
  if(gOptionf) return; ff=fopen(fn,"w");  /* PamCC() */
  if(ff==NULL) { F"Can't write %s.\n",fn); exit(2); }
  FF"/*------------------------------------------------------------------\n");
  /* Q05 */
  FF"FILE:         %s_i.cc\n",StrippedInFileName(0));
  FF"DESCRIPTION:  Interface functions for %s\n",gPam);
  FF"AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov\n");
  FF"BUGS:         Should be automatically generated\n");
  FF"HISTORY:      putDateHere-v000a-hpl- Creation.\n");
  FF"*/\n");
  FF"/*------------------------------------------- INCLUDES            */\n");
  FF"#include <stdio.h>\n");
  FF"#include <stdlib.h>\n");
  FF"#include <string.h>\n");
  /* FF"#include \"ASU.h\"\n"); 960529d */
  /* FF"#include \"AMI.h\"\n"); 960529e */
  /* Q06 */
  FF"#include \"%s.h\"\n",StrippedInFileName(0));
  FF"/*------------------------------------------ TYPEDEFS             */\n");
  FF"/*------------------------------------------ GLOBALS              */\n");
  FF"/*------------------------------------------ PROTOTYPES           */\n");
  FF"/*\n");
  FF"*:>----------------------------------------------------------------\n");
  /* Q07 */
  FF"*:ROUTINE:      STAFCV_T %s_load_ami()\n",gPam);
  /* Q08 */
  FF"*:DESCRIPTION:  Initialize %s\n",gPam);
  FF"*:ARGUMENTS:    amiBroker *broker       - broker for AMI object\n");
  FF"*:RETURN VALUE: TRUE or FALSE\n");
  FF"*:<----------------------------------------------------------------\n");
  FF"*/\n");
  /* Q09 */
/*
  FF"const char *%s_version() {\n",gPam);
  FF"  const char *v=\"@(#) $%s$\";\n","Id:");
  FF"  return v;\n");
  FF"}\n");
*/
  FF"STAFCV_T %s_load_ami(amiBroker *broker)\n{\n",gPam);
  FF"%s_FT %s_call = %s_;\n",gPamUp,gPam,gPam); /* pam_i.cc */
  /* FF"  printf(\"%s_load_ami: Starting ####\");\n",gPam); 960606 */
  /* FF"  printf(\"####################\\n\");\n"); 960606 */
  FF"  STRING_SEQ_T specs;\n");
  FF"  specs._length = specs._maximum = %s_RANK;\n",gPamUp);
  FF"  specs._buffer = new char*[%s_RANK];\n",gPamUp);
  FF"\n");
  for(ii=0;ii<gNArgName[gNProto-1];ii++) {
    FF"  specs._buffer[%d] = new char[strlen(%s_SPEC)+1];\n",ii,
    Up(gDataType[gNProto-1][ii]));
    FF"  strcpy(specs._buffer[%d],%s_SPEC);\n",ii,
    Up(gDataType[gNProto-1][ii]));
  }
  FF"\n");
  FF" broker->deleteInvoker(\"%s\");  broker->newInvoker(\"%s\",%s_RANK\n",gPam,gPam,gPamUp);


  FF"               ,(FNC_PTR_T)%s_call ,specs             );\n",gPam);
/*
  FF"               ,(FNC_PTR_T)%s_call ,specs,%s_version());\n",gPam,gPam);
*/


  FF"  for( int i=0;i<specs._maximum;i++ ){\n");
  FF"     delete specs._buffer[i];\n");
  FF"  }\n");
  FF"  delete[] specs._buffer;\n");
  /* FF"  printf(\"%s_load_ami: Normal conclusion ##\");\n",gPam); 960606 */
  FF"  printf(\"%s module loaded\\n\");\n",gPam);
  /* FF"  printf(\"\\n\");\n"); 960606 */
  FF"  return TRUE;\n");
  FF"}\n");
  fclose(ff); if (!gOptionq) F"  out: %s\n",fn);
}
char *Xidl(char *x) {
  char *cc;
  static char rv[123];
  strncpy(rv,x,120); rv[120]=0;
  cc=strstr(rv,".idl"); if(cc) cc[0]=0;
  return rv;
}
char *Capitalized(char *x) {
  static char rv[123];
  strncpy(rv,x,122); rv[121]=0;
  if(rv[0]<='z'&&rv[0]>='a') rv[0]+='A'-'a';
  return rv;
}
char *Nq(char *x) {
  static char rv[123];
  char *cc;
  if(x[0]=='"') strncpy(rv,x+1,121);
  else strncpy(rv,x,121);
  rv[122]=0;
  cc=strstr(rv,"\""); if(cc) cc[0]=0;
  return rv;
}
void FirstRootPamFile(FILE *ff) {
  char *cc,Sifn[123],*sifn; int i, ii, jj;
  sifn=StrippedInFileName(0); strcpy(Sifn,sifn);
  if(Sifn[0]>='a'&&Sifn[0]<='z') Sifn[0]+='A'-'a';
  FF"#ifndef STAF_St_%s_Module\n",sifn);
  FF"#define STAF_St_%s_Module\n",sifn);
  FF"\n");
  FF"#include \"St_Module.h\"\n");
  FF"\n");
  FF"#ifdef __CINT__\n");
  FF" class table_head_st;\n");
    for(i=1;i<gNIncFile;i++) FF" class %s_st;\n",Xidl(Nq(gIncFile[i])));
    for(i=1;i<gNIncFile;i++) FF" class St_%s;\n",Xidl(Nq(gIncFile[i]))); 
  FF"#else\n");
  for(i=1;i<gNIncFile;i++) 
      FF"#include \"tables/St_%s_Table.h\"\n",Xidl(Nq(gIncFile[i])));
  FF"#endif\n");
  FF"\n");
  FF"\n");
  FF"class St_%s : public St_Module\n",sifn);
  FF"{\n");
  FF"public:\n");
  FF"  St_%s() : St_Module(){}\n",sifn);
  FF"\n");
  FF"// Passing the \"simple\" structures\n");
  FF"\n");
  ii=0;
  if (gNArgName[ii] > 0 ) {
  FF" typedef enum { \n");
/*  for(i=1;i<gNIncFile;i++) {
    cc=Capitalized(Xidl(Nq(gIncFile[i]))); FF"    k%s_h, k%s_d",cc,cc);
    if(i<gNIncFile-1) FF","); FF"\n");
    } */

ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
        cc=Capitalized(gDataType[ii][jj]); FF"    k%s%d",cc,jj);
        if(jj<gNArgName[ii]-1) FF","); FF"\n");
      }
  FF" } E%s;\n",sifn);
  FF"\n");
    FF"// Passing the C++ objects\n");
    FF"\n");
    FF"  St_%s(\n",sifn);
/*    for(i=1;i<gNIncFile;i++) {
       FF"     St_%s *o%d",Xidl(Nq(gIncFile[i])),i);
       if(i<gNIncFile-1) FF","); FF"\n");
       } */
      ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
       FF"     St_%s *o%d",gDataType[ii][jj],jj);
       if(jj<gNArgName[ii]-1) FF","); FF"\n");
      }
    FF"  );\n");
    FF"  Int_t operator()(\n");
/*    for(i=1;i<gNIncFile;i++) {
       FF"     St_%s *o%d",Xidl(Nq(gIncFile[i])),i);
       if(i<gNIncFile-1) FF","); FF"\n");
       }*/
      ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
       FF"     St_%s *o%d",gDataType[ii][jj],jj);
       if(jj<gNArgName[ii]-1) FF","); FF"\n");
      }
    FF"  );\n");
  }
  FF"\n");
  FF"// Calling the wrapped STAF module\n");
  FF"\n");
  FF" Int_t ExecuteModule();\n");
  FF"\n");
  FF"  const Char_t *GetName(){return \"%s\";}\n",sifn);
  FF" ClassDef(St_%s,0) // class-wrapper to cal %s module \n",sifn,sifn);
  FF"};\n");
  FF"\n");
  FF"\n");
  FF"R__EXTERN St_%s &%s; \n",sifn,sifn);
  FF"\n");
  FF"#endif\n");

}
void SecondRootPamFile(FILE *ff) {
  char *sifn,cap[123],uppercase[123]; int i;
  int ii=0; int jj=0;
  sifn=StrippedInFileName(0);
  FF"#include \"%s.h\"\n",StrippedInFileName(0));
  FF"#include \"St_%s_Module.h\"\n",StrippedInFileName(0));
  for(i=1;i<gNIncFile;i++) {
    FF"#include \"tables/St_%s_Table.h\"\n",Xidl(Nq(gIncFile[i])));
/*    if(i<gNIncFile-1) FF","); FF"\n"); */
  }
  FF"\n");

  /* FF"St_%s g%s = new St_%s;\n",sifn,sifn,sifn); */
  FF"St_%s g%s;\n",sifn,sifn);

  FF"St_%s &%s = g%s;\n",sifn,sifn,sifn);
  FF"\n");
  FF"ClassImp(St_%s)\n",sifn);
  FF"\n");
if (gNArgName[ii] > 0 ) { 
    FF"//*-* Passing the C++ objects\n");
    FF"\n");
    FF"//_______________________________________________________________\n");
    FF"St_%s::St_%s(\n",sifn,sifn);
/*yf    for(i=1;i<gNIncFile;i++) {
       FF"  St_%s *o%d",Xidl(Nq(gIncFile[i])),i);
       if(i<gNIncFile-1) FF","); FF"\n");
       } */
      ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
        FF"  St_%s *o%d",Nq(gDataType[ii][jj]),jj);
        if(jj<gNArgName[ii]-1) FF","); FF"\n");
       }
    FF") \n");
    FF" : St_Module(\n");
/*yf    for(i=1;i<gNIncFile;i++) {
       FF"   o%d->GetHeader(),o%d->GetTable()\n",i,i);
       if(i<gNIncFile-1) FF","); FF"\n");
       } */

      ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
       FF"   o%d\n",jj);
       if(jj<gNArgName[ii]-1) FF","); FF"\n");
      }
    FF"   ){}\n");
  FF"//_______________________________________________________________\n");
  FF"Int_t St_%s::operator()(\n",sifn);
/*yf  for(i=1;i<gNIncFile;i++) {
     FF"  St_%s *o%d",Xidl(Nq(gIncFile[i])),i);
     if(i<gNIncFile-1) FF","); FF"\n");
     } */
   ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
      FF"  St_%s *o%d",Nq(gDataType[ii][jj]),jj);
      if(jj<gNArgName[ii]-1) FF","); FF"\n");
}
  FF"  )\n");
  FF"{return InvokeModule(");
/*yf  for(i=1;i<gNIncFile;i++) {
     FF"o%d",i);
     if(i<gNIncFile-1) FF",");
     } */
ii=0;for(jj=0;jj<gNArgName[ii];jj++) {FF"o%d",jj); if(jj<gNArgName[ii]-1) FF",");}
  FF");}\n");
}
  FF"//_______________________________________________________________\n");
  FF"Int_t St_%s::ExecuteModule()\n",sifn);
  FF"{\n");
  FF" //*-* Calling the wrapped STAF '%s' module\n",sifn);
  FF"\n");
if (gNArgName[0]){
  FF" const Char_t *names[]={");
  ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
  FF"\"%s\"",Nq(gDataType[ii][jj]));
  if(jj<gNArgName[ii]-1) FF",\n");
};
  FF"};\n");
  FF"  CheckParameters(names);\n");
};
  FF"Int_t res = ::%s_(\n",sifn);
/*  for(i=1;i<gNIncFile;i++) {
     ToUpper(uppercase,Xidl(Nq(gIncFile[i])));
     strcpy(cap,Capitalized(Xidl(Nq(gIncFile[i]))));
     FF" (TABLE_HEAD_ST *)GetParams(k%s_h), (%s_ST *)GetParams(k%s_d)",
         cap,uppercase,cap);
     if(i<gNIncFile-1) FF","); FF"\n");
     } */
    ii=0;for(jj=0;jj<gNArgName[ii];jj++) {
       ToUpper(uppercase,Nq(gDataType[ii][jj]));
       strcpy(cap,Capitalized(Nq(gDataType[ii][jj])));
       FF" GetHeader(k%s%d), (%s_ST *)GetStruct(k%s%d)",
         cap,jj,uppercase,cap,jj);
       if(jj<gNArgName[ii]-1) FF","); FF"\n");
       }

  FF" );\n");
if (gNArgName[0]){
  FF"res =  CheckResults(res,names);\n");
};
  FF"return res;\n",sifn);
  FF"};\n");
  FF"\n");
  FF"//_______________________________________________________________\n");
  FF"void St_%s::Streamer(class TBuffer &){}\n",sifn);
}
void WriteTheTwoRootPamFiles(void) {
  FILE *ff; char fn[123];

  sprintf(fn,"St_%s_Module.h",StrippedInFileName(0));
  ff=fopen(fn,"w"); if(!ff) ERR; FirstRootPamFile(ff); fclose(ff); if (!gOptionq) F"  out: %s\n",fn);

  sprintf(fn,"St_%s_Module.cxx",StrippedInFileName(0));
  ff=fopen(fn,"w"); if(!ff) ERR; SecondRootPamFile(ff); fclose(ff);
}
void PamTemplateC(void) {
  int type,lookfor;
  char headerName[111],tableType[111],fn[100]; FILE *ff; int ii,jj,initOrCall;
  char *label,sh[89],st[89],comma[17],fName[111],withAsterisk[79];
  /* Q10 */
  sprintf(fn,"%s.c.template",StrippedInFileName(0));
  if(gOptionM || gOptionT) { ExtendOutList(fn); return; }
  if(gOptionf) return; ff=fopen(fn,"w"); /* PamTemplateC() */
  if(ff==NULL) { F"Can't write %s.\n",fn); exit(2); }
  TemplateCBegin(ff);
  /* Q11 */
  FF"#include \"%s.h\"\n\n",StrippedInFileName(0));
  /* Q12 */
  FF"long type_of_call %s_(\n",gPam);
  for(ii=0;ii<gNProto;ii++) {
    if(!strcmp(gPn[ii],"initialize")) initOrCall=INIT;
    else if(!strcmp(gPn[ii],"call")) initOrCall=CALL;
    else Err(__LINE__);
    if(initOrCall==CALL) {
      for(jj=0;jj<gNArgName[ii];jj++) {
        if(jj==gNArgName[ii]-1) strcpy(comma,")\n{"); else strcpy(comma,",");
        sprintf(headerName,"*%s_h",gArgName[ii][jj]);
        sprintf(tableType,"%s_ST",Up(gDataType[ii][jj]));
        if(jj==0) { strcpy(sh,headerName); strcpy(st,gArgName[ii][jj]); }
        sprintf(withAsterisk,"*%s",gArgName[ii][jj]);
        FF"  TABLE_HEAD_ST %17s, %17s %17s %s\n",
        headerName,tableType,withAsterisk,comma);
      }
    }
  }
  TemplateCMiddle(gInFileNameNoPath,ff);
  for(ii=0;ii<gNProto;ii++) {
    if(!strcmp(gPn[ii],"initialize")) initOrCall=INIT;
    else if(!strcmp(gPn[ii],"call")) initOrCall=CALL;
    else Err(__LINE__);
    if(initOrCall==CALL) {
      for(type=0;type<3;type++) {
        switch(type) {
          case 0: lookfor=IO_IN; label="IN"; break;
          case 1: lookfor=IO_INOUT; label="INOUT"; break;
          case 2: lookfor=IO_OUT; label="OUT"; break;
        }
        FF"**:    %5s:\n",label);
        for(jj=0;jj<gNArgName[ii];jj++) {
          if(gIo[0][jj]!=lookfor) continue;
          FF"**:  %17s    - PLEASE FILL IN DESCRIPTION HERE\n",
          gArgName[ii][jj]);
          FF"**:  %16s_h   - header Structure for %s\n",
          gArgName[ii][jj], gArgName[ii][jj]);
        }
      }
    }
  }
  TemplateCEnd(ff);
  fclose(ff); 
  if (!gOptionq) 
     F"  out: %s\n",fn);
}
void GenerateStaticLenght(const char *in,char *out) {
  strcpy(out,in); strcat(out,"_h.maxlen");
}
void PamOutputDotIncFile(void) {
  char *cc,theType[111],tmp[99],ooo[99]; int doup,initOrCall,ii,jj;
  char havePam=0,tableType[100],headerName[100],buf[83];
  FILE *ff;
  /* Q13 */
  ff=OpenOnePamOutput(".inc"); if(gOptionM || gOptionT) return;
  FF"C   %s.inc\n",StrippedInFileName(0));
  FF"#ifndef %s_INC\n",StrippedInFileName(7));
  FF"#define %s_INC\n",StrippedInFileName(7));
  FF"#endif\n");
  for(ii=0;ii<gNIncFile;ii++) {
    if(!strcmp(gIncFile[ii],"\"PAM.idl\"")) havePam=7;
  }
  if(!havePam) FF"#include \"%s.inc\"\n","PAM"); /* 960529f */
  for(ii=0;ii<gNIncFile;ii++) {
    strcpy(tmp,gIncFile[ii]); doup=0;
    cc=strstr(tmp,".idl"); if(cc!=NULL) *cc=0; /* dont del, modifies tmp */
    if(doup) ToUpper(ooo,tmp); else strcpy(ooo,tmp);
    if(strcmp(ooo,"ASU")&&strcmp(ooo,"asu")) FF"#include %s.inc\"\n",ooo);
  }
  if(gNProto!=1) Err(__LINE__);
  if(gNArgName[gNProto-1]<0) Err(__LINE__);
  FF"      INTEGER*4 %s_RANK\n",gPamUp);
  FF"      PARAMETER (%s_RANK=%d)\n",gPamUp,gNArgName[gNProto-1]);
  for(ii=0;ii<gNProto;ii++) {	/* moved from .F file 960215 */
    if(!strcmp(gPn[ii],"initialize")) initOrCall=INIT;
    else if(!strcmp(gPn[ii],"call")) initOrCall=CALL;
    else Err(__LINE__);
    if(initOrCall==CALL) {
      for(jj=0;jj<gNArgName[ii];jj++) {
        sprintf(headerName,"%s_h",gArgName[ii][jj]);
        sprintf(tableType,"/%s_ST/",Up(gDataType[ii][jj]));
        FF"      RECORD %20s %23s\n","/TABLE_HEAD_ST/",headerName);
        if(gOptionstatic) {
          GenerateStaticLenght(gArgName[ii][jj],buf);
          FF"      RECORD %20s %20s(%s)\n",tableType,gArgName[ii][jj],buf);
        } else if(gOptiondynamic) {
          sprintf(buf,"%s_d",gArgName[ii][jj]);
          FF"      RECORD %20s %20s(*)\n",tableType,buf);
          FF"      POINTER (%s,%s)\n",gArgName[ii][jj],buf);
        } else {
          FF"      RECORD %20s %20s(*)\n",tableType,gArgName[ii][jj]);
        }
      }
    }
  }
  fclose(ff);
}
#define MARGIN 2
void Banner(char *xx) { /* makes a big obnoxious banner from input x */
  int ii,line,len; char x[100];
  sprintf(x," %s ",xx);
  len=strlen(x);
  PP"%c",7);
  if(len+2*MARGIN>80) {
    PP"%s\n",x);
  } else {
    for(line=0;line<7;line++) {
      for(ii=MARGIN-1;ii>=0;ii--) PP"X");
      switch(line) {
        case 0: case 1: case 5: case 6: for(ii=len-1;ii>=0;ii--) PP"X"); break;
        case 2: case 4: for(ii=len-1;ii>=0;ii--) PP" "); break;
        case 3: PP"%s",x);
      }
      for(ii=MARGIN-1;ii>=0;ii--) PP"X");
      PP"\n");
    }
  }
  sleep(5);
}
void CheckForPamIdl(void) {
  if(gHaveIncludedPamIdl) return;
  if(gHaveSeen_STAFCV_T) {
    Ose();
    printf("XX\n");
    printf("XX Fatal error.\n");
    printf("XX (semantic): Identifier 'STAFCV_T' undefined.\n");
    printf("XX #include \"PAM.idl\" as 1st include.\n");
    printf("XX\n");
    exit(2);
  }
  if(gHaveSeen_amiModule) {
    Ose();
    printf("XX\n");
    printf("XX Fatal error.\n");
    printf("XX (semantic): Identifier 'amiModule' undefined.\n");
    printf("XX #include \"PAM.idl\" as 1st include.\n");
    printf("XX\n");
    exit(2);
  }
}
void CheckThatAllTablesHaveBeenIncluded(void) {
  int i,j; 
  char ok;
  for(i=0;i<gNmoduleTable;i++) {
    ok=0;
    for(j=0;j<gNinputTable;j++) {
      if(!strcmp(gModuleTable[i],gInputTable[j])) { ok=7; break; }
    }
    if(!ok && !gOptions && !gOptionq) {
      Ose();
      PP"%cWARNING FROM STIC:  you did not include an IDL file ",7);
      PP"for table\n");
      PP"type '%s', which is mentioned in %s.\n",
      gModuleTable[i],gOrigInputFile); 
    }
  }
}
void Pam(void) {
                               CheckForPamIdl();
                               /*DumpGlobalsPam();*/
  if( !gOptiont              ) PamOutputDotIncFile();
  if( !gOptiont              ) PamOutputDotHFile();
                               /*PamOutputDotHHFile();*/
  if(              !gOptionH ) PamTemplateC();
  if( !gOptiont && !gOptionH ) PamCC();
  if(              !gOptionH ) PamTemplateFortran();
  if(gOptionr) WriteTheTwoRootPamFiles();
}
IncludeFileName(char *io) {
  if(gNIncFile>=INC) { F"Too many include files, max=%d.\n",INC); exit(2); }
  strncpy(gIncFile[gNIncFile++],io,ISIZE);
}
InOut(char *io) {  /* STEP 1 */
  int whichProto; whichProto=gNProto-1;
  if(gNProto<1) Err(__LINE__);
  if(gNArgName[whichProto]>=ARGS) {
    F"No more than %d arguments per prototype.\n",ARGS); exit(2);
  }
  if(!strcmp(io,"in")) gIo[whichProto][gNArgName[whichProto]]=IO_IN;
  else if(!strcmp(io,"out")) gIo[whichProto][gNArgName[whichProto]]=IO_OUT;
  else if(!strcmp(io,"inout")) gIo[whichProto][gNArgName[whichProto]]=IO_INOUT;
  else Err(__LINE__);
}
ArgType(char *theName) {  /* STEP 2 */
  int whichProto; whichProto=gNProto-1;
  if(strlen(theName)>MODULETABSIZE) ERR;
  if(gNmoduleTable>=MODULETAB) ERR;
  strcpy(gModuleTable[gNmoduleTable++],theName);
  if(gNProto<1) Err(__LINE__);
  if(gNArgName[whichProto]>=ARGS) {
    F"No more than %d arguments per prototype.\n",ARGS); exit(2);
  }
  strncpy(gDataType[whichProto][gNArgName[whichProto]],theName,TSIZE);
  gDataType[whichProto][gNArgName[whichProto]][TSIZE]=0;
}
ArgName(char *theName) {  /* STEP 3 */
  int whichProto; whichProto=gNProto-1;
  if(gNProto<1) Err(__LINE__);
  if(gNArgName[whichProto]>=ARGS) {
    F"No more than %d arguments per prototype.\n",ARGS); exit(2);
  }
  strncpy(gArgName[whichProto][gNArgName[whichProto]++],theName,ISIZE);
}
/*--------------------------------------------  961113
void CheckForEqualityWithInputName(void) {
  char xxx[123],yyy[123];
  StripOffIdl(gInFileNameNoPath,xxx);
  if(strcmp(xxx,gPam)) {
    Ose();
    EE"Fatal error in file %s:\n",gInFileName);
    EE"The name of the interface (%s) does not\n",gPam);
    EE"match the file name (%s).\n",gInFileNameNoPath);
    EE"Either rename the file %s.idl, or\n",gPam);
    EE"change the name of the interface in the file to %s.\n",xxx);
    EE"Eg: interface %s : ...\n",xxx);
    exit(2);
  }
}
----------------------------------------------------*/
PamName(char *theName) {
  gNPamNames++;
  if(gNPamNames>1) { F"Only one interface definition per file.\n"); exit(2); }
  strncpy(gPam,yylval.str,ISIZE);
  /* 961113 CheckForEqualityWithInputName(); */
  ToUpper(gPamUp,gPam);
}
FncType(char *theName) {
  if(gNProto>=PROTOTYPES) {
     F"Too many prototype names (max %d).\n",PROTOTYPES); exit(2);
  }
  strncpy(gFncType[gNProto],theName,TSIZE);
}
PrototypeName(char *theName) {
  if(gNProto>=PROTOTYPES) {
     F"Too many prototype names (max %d).\n",PROTOTYPES); exit(2);
  }
  strncpy(gPn[gNProto++],theName,ISIZE);
}
void Init(void) {
  int ii,jj;

  gNoMoreComments=0; *gComments=0;

  gLN=1; gNPamNames=0; gNProto=0; gC=7; gNColTypes=0;
  gNColNames=0; gNIncFile=0; gNTblName=0;

  for(ii=PROTOTYPES-1;ii>=0;ii--) gNArgName[ii]=0;

  gTable[0]='\0'; gL2[0]='\0'; gL1[0]='\0'; gPam[0]='\0';
  gPamUp[0]='\0';

  for(ii=COL-1;ii>=0;ii--) gColName[ii][0]='\0';
  for(ii=PROTOTYPES-1;ii>=0;ii--) gIo[ii][0]='\0';
  for(ii=INC-1;ii>=0;ii--) gIncFile[ii][0]='\0';
  for(ii=PROTOTYPES-1;ii>=0;ii--) gPn[ii][0]='\0';
  for(ii=COL-1;ii>=0;ii--) gColType[ii][0]='\0';
  for(ii=PROTOTYPES-1;ii>=0;ii--) gFncType[ii][0]='\0';


  for(ii=PROTOTYPES-1;ii>=0;ii--) {
    for(jj=ARGS-1;jj>=0;jj--) {
       gArgName[ii][jj][0]='\0';
      gDataType[ii][jj][0]='\0';
    }
  }
}
void Help(void) {
 P"For help is using STIC with STAF, write CETull@lbl.gov or\n");
 P"ward@physics.utexas.edu\n");
 Usage();
}
void Usage(void) {
  F"Usage: %s [-h?rMTivftq] [-Iincdir] [-static|-dynamic] [xxx.idl]\n",
  gExeName);
  F"All options are optional.\n");
  F"\n");
  F"You can type multiple options on the command line with a single\n");
  F"dash (eg, %s -ti xxx.idl).\n",gExeName);
  F"This does not imply that all option combinations are sensible.\n");
  F"\n");
  F"You don't need an input idl file for options h and v.\n");
  F"\n");
  F"-? Prints this usage message and then immediately quit.\n");
  F"-dynamic Dynamic tables.\n");
  F"-f Produce only header files (.h and .inc).\n");
  F"-h Prints this usage message and then immediately quit.\n");
  F"-H Produce only the header   files.\n");
  F"-i Ignore case (upper converted to lower).\n");
  F"-I Mechanism for specifying list of include directories.\n");
  F"-M Write string to stdout for use in a Makefile, no other output.\n");
  F"-s Do not process include files in module idl files.\n");
  F"-T Write string with used tables to stdout for use in a Makefile, no other output.\n");
  F"-q Operate quietly.\n");
  F"-static  Static tables.\n");
  F"-t Produce only the template files.\n");
  F"-r Produce only the ROOT files.\n");
  F"-v Write version info to stdout, no other output is produced.\n");
  F"-version XXX adds an arbitrary version XXX to the output file.\n");
  exit(2);
}
void OpenAllTblOutput(void) {
  char mode[3],*xx,*cc,fn[111],zz[111]; int ii;
  strcpy(zz,gInFileName);
  for(ii=strlen(zz);ii>=0;ii--) if(zz[ii]=='/') break; xx=zz+ii+1;
  cc=strstr(xx,".idl"); if(cc==NULL) Usage(); cc[0]=0;

  {sprintf(fn,"%s.inc",xx); ModeFromFn(fn,mode);}
  if(gOptionM || gOptionT ) {
    ExtendOutList(fn);
  } else {
    gFpInc=fopen(fn,mode); /* gOptionf OK */
    if(gFpInc==NULL) { F"Can't write %s.\n",fn); exit(2); }
    if(!gOptionM&&!gOptionT&&!strcmp(mode,"w")&&!gOptionq) P"  out: %s\n",fn);
    StandardBlurb(1,mode,gFpInc);
  }

  sprintf(fn,"%s.h",xx); ModeFromFn(fn,mode);
  if(gOptionM || gOptionT ) {
    ExtendOutList(fn);
  } else {
    gFpH=fopen(fn,mode); /* gOptionf OK */
    if(gFpH==NULL) { F"Can't write %s.\n",fn); exit(2); }
    if(!gOptionM&&!gOptionT&&!strcmp(mode,"w")&&!gOptionq) P"  out: %s\n",fn); 
    StandardBlurb(2,mode,gFpH);
  }
}
int FirstCharsSame(const char *x,const char *y) {
  int len1,ii,len2;
  len1=strlen(x); len2=strlen(y);
  if(len2<len1) len1=len2;
  for(ii=len1-1;ii>=0;ii--) { if(x[ii]!=y[ii]) return 0; }
  return 7;
}
void SetYyinFilePtr(char *xx) {
  FILE *ff; char line[203]; int qq,ii;
  yyin=fopen(xx,"r");
  if(yyin==NULL) {
    F"I can't read %s.  Check existence and permissions.\n",xx); exit(2);
  }
  if(gOptioni) {
    ff=fopen(gOptioniTempFile,"w"); /* gOptionf OK */
    if(!ff) { PP"Fatal error: can't write %s.\n",gOptioniTempFile); exit(2); }
    while(fgets(line,200,yyin)) {
      qq=0;
      for(ii=0;line[ii];ii++) {
        if(line[ii]=='\"') qq++; if(qq%2==1) continue;
        if(FirstCharsSame(line+ii,"STAFCV_T")) { ii+=8; continue; }
        if(line[ii]>='A'&&line[ii]<='Z') line[ii]+='a'-'A';
      }
      fprintf(ff,"%s",line);
    } fclose(ff); fclose(yyin);
    yyin=fopen(gOptioniTempFile,"r");
  }
}
void DoOneLineComment(char *x) {
  int ii;
  if(gNColNames>0&&gNColNames<=COL&&gFtc>=0&&gFtc<COL) {
    for(ii=gFtc;ii<gNColNames;ii++) {
      if(strlen(x)>=OLC) Err(__LINE__);
      if(ii==gFtc) strcpy(gOlc[ii],x); else strcpy(gOlc[ii],"/* ditto */");
    }
  }
}
void CheckSelfConsistencyOfOptions(void) {
  if(gOptionstatic&&gOptiondynamic) {
    Ose(); P"Don't use both -dynamic and -static on command line.\n"); exit(2);
  }
}
void TooManyIncs(void) {
  EE"You have too many \"-I\"s on the command line.  Max=%d.\n",INCDIR);
}
void PrintVersionAndExit(void) {
  printf("%s\n",gCvsVersion); exit(0);
}
void TypeIncDirs(FILE *xx) {
  int ii;
  fprintf(xx,"------------------------\n");
  for(ii=0;ii<gNincDir;ii++) fprintf(xx,"%s/\n",gIncDir[ii]);
  fprintf(xx,"------------------------\n");
 }
void DumpOptionsAndExit(void) {
  PP"%20s %d\n","gOptionM",gOptionM);
  PP"%20s %d\n","gOptionT",gOptionT);
  PP"%20s %d\n","gOptionf",gOptionf);
  PP"%20s %d\n","gOptiondynamic",gOptiondynamic);
  PP"%20s %d\n","gOptioni",gOptioni);
  PP"%20s %s\n","gOptioniTempFile",gOptioniTempFile);
  PP"%20s %d\n","gOptionstatic",gOptionstatic);
  PP"%20s %d\n","gOptionH",gOptionH);
  PP"%20s %d\n","gOptions",gOptions);
  PP"%20s %d\n","gOptiont",gOptiont);
  exit(2);
}
void ReadOptions(int nnn,char *aaa[]) {
  int jj,filenameCount=0,ii; char die=0;
  gOptionstatic=0; gOptiondynamic=0; gOptionM=0; gOptionT=0; gOptioni=0; gOptionH=0; gOptions=0;
  gOptiont=0; gOptionf=0; gOptionr=0;
  gOptionq = 0;
  gNincDir=0; strcpy(gIncDir[gNincDir++],".");
  for(ii=1;ii<nnn;ii++) {
    if(aaa[ii][0]=='-') {
           if(!strcmp(aaa[ii]+1,"dynamic")) gOptiondynamic=7;
      else if(!strcmp(aaa[ii]+1,"static"))  gOptionstatic=7;
      else if(!strcmp(aaa[ii]+1,"version")){
        ii++;
        (void) strcpy(VERSION,aaa[ii]);
        /* (void) printf("Got version %s\n",aaa[ii]); */
      }
      else if(aaa[ii][1]=='I') {
        if(gNincDir>INCDIR) TooManyIncs();
        strcpy(gIncDir[gNincDir++],aaa[ii]+2);
      } else {  /* single-letter options may be combined (eg, -it) */
        for(jj=1;aaa[ii][jj];jj++) {
               if(aaa[ii][jj]=='H') gOptionH=7;
          else if(aaa[ii][jj]=='s') gOptions=7;
          else if(aaa[ii][jj]=='t') gOptiont=7;
          else if(aaa[ii][jj]=='M') gOptionM=7;
          else if(aaa[ii][jj]=='T') gOptionT=7;
          else if(aaa[ii][jj]=='f') gOptionf=7;
          else if(aaa[ii][jj]=='i') gOptioni=7;
          else if(aaa[ii][jj]=='r') gOptionr=7;
          else if(aaa[ii][jj]=='q') gOptionq=7;
          else if(aaa[ii][jj]=='h') Usage();
          else if(aaa[ii][jj]=='?') Usage();
          else if(aaa[ii][jj]=='v') PrintVersionAndExit();
          else {
            Ose(); PP"Unknown option: %s\n",aaa[ii]); Usage();
          }
        }
      }
    } else {      /* command line arg aaa[ii] is the input file name */
      for(jj=strlen(aaa[ii])-1;jj>=0;jj--) if(aaa[ii][jj]=='/') break;
      strncpy(gInFileNameNoPath,aaa[ii]+jj+1,INFILES);
      strcpy(gOrigInputFile,aaa[ii]+jj+1);
      strncpy(      gInFileName,aaa[ii],     INFILES);
      if (!gOptionq) 
        PP"Input file %s\n",gInFileNameNoPath);
      filenameCount++;
    }
  }
  if(filenameCount>1) P"You specified too many input files.\n");
  if(filenameCount<1) P"You did not specify an input file.\n");
  if(die||filenameCount!=1) Usage();
  CheckSelfConsistencyOfOptions();
  /* DumpOptionsAndExit(); */
}
void Init2(void) {
  int ii;
  for(ii=COL-1;ii>=0;ii--) gOlc[ii][0]=0;
}
#define INCFILES 50 /* max length of included idl file's name */
void NoFindIncFile(char *incFile,const char *curFile) {
  Eose(); EE"Fatal error in STIC.  Could not find\n");
  EE"%s, ",incFile);
  EE"which was included in idl file %s.\n",curFile);
  EE"I searched the following directories:\n");
  TypeIncDirs(stderr);
  EE"To add to this list of directories, use my -I option.\n");
  Eose();
  exit(2);
}
void PreParseScanOfFile(void) {
  char line[302];
  while(fgets(line,300,yyin)) {
    if(strstr(line,"PAM.idl"))   gHaveIncludedPamIdl=7;
    if(strstr(line,"amiModule")) gHaveSeen_amiModule=7;
    if(strstr(line,"STAFCV_T"))  gHaveSeen_STAFCV_T=7;
  }
}
void RecursiveProcessingOfIncludeFiles(const char *curFile) {
  char save,line[225],*incFile,incFileFullPath[225]; int ii,quote;
  char cheapFix[20];
  FILE *incFileFp,*curFileFp;
  curFileFp=fopen(curFile,"r");
  if(!curFileFp) { EE"No can read %s.\n",curFile); exit(2); }
  while(fgets(line,222,curFileFp)) {
    save=line[10]; line[10]=0; strncpy(cheapFix,line,15); cheapFix[9]=0;
    if(!strcmp(cheapFix,"#include ")) {
      line[10]=save; for(ii=0;line[ii];ii++) if(line[ii]=='\"') break;
      if(line[ii]!='\"') { PP"Did not find quotes %s\n",line); exit(2); }
      quote=ii; strtok(line+quote+1,"\""); incFile=line+quote+1;
      if(strstr(incFile,".idl")) {
        if(!gOptionM && !gOptionT && !gOptionq) PP"Processing %23s included in %23s\n",incFile,curFile);
        for(ii=0;ii<gNincDir;ii++) {
          sprintf(incFileFullPath,"%s/%s",gIncDir[ii],incFile);
          incFileFp=fopen(incFileFullPath,"r");
          if(incFileFp) {
            fclose(incFileFp); HandleOneInputFile(incFileFullPath); break;
          }
        } if(ii>=gNincDir) NoFindIncFile(incFile,curFile);
      }
    }
  } fclose(curFileFp);
}
void HandleOneInputFile(char *inFile) { /* maybe inFile=gInFileName */
  char buffer[INFILES+2]; int ii;
  strcpy(buffer,inFile); strcpy(gInFileName,buffer);
  for(ii=strlen(gInFileName)-1;ii>=0;ii--) if(gInFileName[ii]=='/') break;
  strcpy(gInFileNameNoPath,gInFileName+ii+1);
  gFpInc=NULL; gFpH=NULL;

  /* This function not called for PAM.idl unless it is in include path. */
  if(strstr(inFile,"PAM.idl")) { gHaveIncludedPamIdl=7; return; }

  SetYyinFilePtr(gInFileName); PreParseScanOfFile(); fclose(yyin);
  SetYyinFilePtr(gInFileName); 
  Init2(); 
  yyparse();   
  fclose(yyin);
  if(gNInFile>=INFILE) Err(__LINE__);
  if(strlen(inFile)>INFILES) Err(__LINE__);
  strcpy(gInFile[gNInFile++],inFile);
  if(gOptionM || gOptionT) {
    if(gFpInc) Err(__LINE__);
    if(gFpH) Err(__LINE__);
  } else { 
    if (gFpInc)
      fclose(gFpInc); 
    if (gFpH) 
      fclose(gFpH); 
  }
  if(!gOptions) RecursiveProcessingOfIncludeFiles(buffer);
  if(!gOptionM&&!gOptionT&&!gOptionq) PP"----- finished with %s\n",buffer);
} /* save inFile buffer gInFileName */
void FixVersionInfo(void) {
  int ii;
  if(strlen(gCvsVersionRaw)>CVSVERSION) ERR;
  strcpy(gCvsVersion,gCvsVersionRaw);
  for(ii=0;gCvsVersion[ii];ii++) if(gCvsVersion[ii]=='$') gCvsVersion[ii]=' ';
}

int main(int nnn,char *aaa[]) {
  int i;

  VERSION[0]='\0';
  gNmoduleTable=0; gNinputTable=0; FixVersionInfo();
  gHaveSeen_STAFCV_T=0; gHaveSeen_amiModule=0; gHaveIncludedPamIdl=0;
  strncpy(gExeName,aaa[0],EXE); gNOutFile=0;
  sprintf(gOptioniTempFile,"/tmp/stic.option.i.%d",getpid());
  if(nnn<2) Usage();
  ReadOptions(nnn,aaa);
  if(!gOptionM&&!gOptionT&&!gOptionq) P"For help type %s help.\n",gExeName);
  if(!strcmp(gInFileName,"help")) Help();
  HandleOneInputFile(gInFileName);
  CheckThatAllTablesHaveBeenIncluded();
  if(gOptionM) {
    for(i=0;i<gNOutFile;i++) {
      if(strstr(gOutFile[i],"template")) continue;
      PP"%s",gOutFile[i]); if(i<gNOutFile-1) PP" ");
    }
    PP":\t");
    for(i=0;i<gNInFile;i++) { PP"%s",gInFile[i]); if(i<gNInFile-1) PP" "); }
    PP"\n");
  }
  if (gOptionT) {
    for(i=1;i<gNInFile;i++) { PP"%s",gInFile[i]); if(i<gNInFile-1) PP" "); }
    PP"\n");
  }
  fflush(stdout); 
  exit(0);
}
yyerror(char *s) {
}
Error(void) {
  fprintf(stderr,"oooooooooooooooooooooooooooooooooooooooooo\n");
  fprintf(stderr,ERROR_FORMAT,gLN,gL1,gL2);
  for(gIi=strlen(gL2)-1;gIi>=0;gIi--) if(gL2[gIi]==' ') break; gJj=gIi+1;
  for(gIi=0;gIi<strlen(gL2);gIi++) {
    if(gIi<gJj) {
      if(gL2[gIi]!='\t') fprintf(stderr," ");  else fprintf(stderr,"\t");
    }
    else fprintf(stderr,"-");
  }
  fprintf(stderr,"\n");
  fprintf(stderr,"oooooooooooooooooooooooooooooooooooooooooo\n"); exit(2);
}
#define LS strcat(gL2,yytext);
#include "idl-lex.c"
/*
ttd char* types in xx_TBL.idl files, and perhaps also in xx_PAM.idl files.
*/
/* Mnemonicity table.
gLN	line count for error messages
idlFile		sum total
INCLU	#include
INTER	interface keyword "interface"
CORBA	"long", "float", etc., for fnct ret prototypes only
protos		body of idl interface definition, composed of proto's
proto		Eg, "long initialized(in int data_spec,out string cluster);".
args		Eg, "in int data_spec,out string cluster".
arg		Eg, "in int data_spec".
INOUT		"in" or "out".
IDENT	variable or prototype name (a, a2, Alpha, etc.)
NUMBE		12, 15 etc
pam		physics anal. mod.
error		yacc keyword, see yacc doc.
*/
