/***********************************************************  TYPEDEFS  **/
typedef int myBool;
/***********************************************************  INCLUDES  **/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "dstype.h"
#include "dsxdr.h"
#include "dscuts.h"
extern char gStr[100];
/***********************************************************  DEFINES  **/
#define ERR_1 "The number typed in the subscript box is too small.\nMin=1."
#define ERR_2 "The number typed in the subscript box\nis too big.\nMax=%d"
#define ERR_3 "Subscript for column array type is out of range in cuts."
#define ERR_4 "Subscript for column array type is out of range."
#define PP printf(
#define PSIZE 2200
#define COPY 500
#define BITSPERCHAR 8
#define PARSE_ERR         -1
#define FALSE              0
#define TRUE               1
#define OPERATOR_TYPE_PE   0
#define OPERATOR_TYPE_GT   1
#define OPERATOR_TYPE_LT   2
#define OPERATOR_TYPE_GE   3
#define OPERATOR_TYPE_LE   4
#define OPERATOR_TYPE_EQ   5
/***********************************************************  GLOBALS  **/
myBool gTableError,gAlreadyDidIt;
char gCopy[PSIZE];
extern int gDone;
/***********************************************************  DEFINES  **/
#define GRAPHICS 0
#define TEXT 1
#define PROGRESS 2
/***********************************************************  FUNCTIONS  **/
void Say(char *mess);
void Say1(char *x) {
  if(!gAlreadyDidIt) { gAlreadyDidIt=TRUE; Say(x); }
}
void GetLocationAndTypeOfOperator(char *cc,int *location,int *type) {
  char k1,k2; int fo,ii; *type=OPERATOR_TYPE_PE;  /* PE=parse error */
  for(ii=strlen(cc)-1;ii>=0;ii--) {
    if(cc[ii]=='.'&&cc[ii+3]=='.') {
      k1=cc[ii+1]; k2=cc[ii+2]; fo=0;
      if(k1=='g'&&k2=='t') { fo=TRUE; *type=OPERATOR_TYPE_GT; }
      if(k1=='l'&&k2=='t') { fo=TRUE; *type=OPERATOR_TYPE_LT; }
      if(k1=='g'&&k2=='e') { fo=TRUE; *type=OPERATOR_TYPE_GE; }
      if(k1=='l'&&k2=='e') { fo=TRUE; *type=OPERATOR_TYPE_LE; }
      if(k1=='e'&&k2=='q') { fo=TRUE; *type=OPERATOR_TYPE_EQ; }
      if(fo) break;
    }
  }
  *location=ii;
}
int HasLetters(char *cc) {
  int ii;
  for(ii=strlen(cc)-1;ii>=0;ii--) {
    if(cc[ii]>='a'&&cc[ii]<='z') return TRUE;
  }
  return FALSE;
}
int StripSubAndRetItsValue(char *xx) {
  register int ii,len; int rv=0;
  len=strlen(xx);
  for(ii=0;ii<len;ii++) {
    if(xx[ii]=='[') { rv=atoi(xx+ii+1); xx[ii]='\0'; break; }
  }
  if(rv<1) rv=1; return rv-1;
}
float Val(char *cc,DS_DATASET_T *pTable,long row) {
  size_t sizetRow,colNum;
  int dataType; float rv,fv; long iv; int sub;
  if(HasLetters(cc)) {
    sizetRow=row;
    sub=StripSubAndRetItsValue(cc);
    if(!dsFindColumn(&colNum,pTable,cc)) {
      PP"Fatal error: table browser can't find column \"%s\".\n",cc);
      gDone=7; return 0.0;
    }
    if(!TableValue(&dataType,gStr,&fv,&iv,sizetRow,colNum,pTable,sub)) {
      gTableError=TRUE; return 0.0;
    }
    if(dataType==FLOAT) rv=fv;
    else if(dataType==INTEGER) rv=iv;
    else if(dataType==STRING) {
      PP"Fatal error in table browser:  char string used in cuts.\n");
      gDone=7; return 0.0;
    }
    /* PP"Val rets %f.  dataType=%d\n------------------\n",rv,dataType); */
    return rv;
  } else return(atof(cc));
}
float Val1(char *cc,int loc,DS_DATASET_T *pTable,long row) {
  char dd[PSIZE]; strcpy(dd,cc); dd[loc]='\0';
  return Val(dd,pTable,row);
}
float Val2(char *cc,int loc,DS_DATASET_T *pTable,long row) {
  char dd[PSIZE]; strcpy(dd,cc+loc+4);
  return Val(dd,pTable,row);
}
int SingleExpressionResult(DS_DATASET_T *pTable,long row,char *cuts) {
  int location,type;  /* type tells whether .gt., .lt., etc. */
  float v1,v2;
  GetLocationAndTypeOfOperator(cuts,&location,&type);
  v1=Val1(cuts,location,pTable,row);
  v2=Val2(cuts,location,pTable,row);
#ifdef DEBUG
  PP"type=%d (le%d ge%d gt%d lt%d eq%d), val1=%f val2=%f\n",
  type, OPERATOR_TYPE_LE, OPERATOR_TYPE_GE, OPERATOR_TYPE_GT,
  OPERATOR_TYPE_LT, OPERATOR_TYPE_EQ,v1,v2);
#endif
  switch(type) {
    case OPERATOR_TYPE_LE: return(v1<=v2);
    case OPERATOR_TYPE_GE: return(v1>=v2);
    case OPERATOR_TYPE_GT: return(v1> v2);
    case OPERATOR_TYPE_LT: return(v1< v2);
    case OPERATOR_TYPE_EQ: return(v1==v2);
    case OPERATOR_TYPE_PE: Say1("Parse error on cuts.");  return PARSE_ERR;
    default: PP"fatal error in module evl_vs\n");
  }
}
void DelChar(char *cc,int pos) {
  int ii,len; len=strlen(cc);
  for(ii=pos;ii<len;ii++) cc[ii]=cc[ii+1];
}
int NumberOfLeftParens(char *cc) {
  int ii,pp=0;
  for(ii=strlen(cc)-1;ii>=0;ii--) if(cc[ii]=='(') pp++; return pp;
}
int LeftParAtStartIsMateOfRightParAtEnd(char *cc) {
  int len,ii,pcnt;
  pcnt=0; len=strlen(cc);
  for(ii=len-1;ii>=0;ii--) {
    if(cc[ii]==')') pcnt--; if(cc[ii]=='(') pcnt++;
    if(pcnt==0 && ii>0) return FALSE;
  }
  if(pcnt==0) return TRUE;
}
void DelWhiteSpace(char *cc) {
  int didOne,ii,len;
  do {
    len=strlen(cc); didOne=0;
    for(ii=len-1;ii>=0;ii--) {
      if(cc[ii]==' ') { didOne=7; DelChar(cc,ii); break; }
    }
  } while(didOne);
}
void ToLowerCase(char *c) {
  int i;
  for(i=strlen(c)-1;i>=0;i--) { if(c[i]>='A'&&c[i]<='Z') c[i] += 'a'-'A'; }
}
int DelOutParens(char *cc) {
  int len;
  len=strlen(cc);
  if( *cc=='(' && cc[len-1]==')' && LeftParAtStartIsMateOfRightParAtEnd(cc) ) {
    DelChar(cc,0); len=strlen(cc); DelChar(cc,len-1);
  }
  return TRUE;
}
int AtAnd(char *c,int i) {
  if(c[i]=='.'&&c[i+1]=='a'&&c[i+2]=='n'&&c[i+3]=='d'&&c[i+4]=='.') return 7;
  return 0;
}
int AtOr(char *c,int i) {
  if(c[i]=='.'&&c[i+1]=='o'&&c[i+2]=='r'&&c[i+3]=='.') return 7;
  return 0;
}
void SetThisRow(long row,char *ba) {
  long whichChar; int whichBit; char mask;
  whichBit=row%BITSPERCHAR; whichChar=row/BITSPERCHAR;
  switch(whichBit) {
    case 0: mask=1; break;
    case 1: mask=2; break;
    case 2: mask=4; break;
    case 3: mask=8; break;
    case 4: mask=16; break;
    case 5: mask=32; break;
    case 6: mask=64; break;
    case 7: mask=128; break;
    default: mask=0;
  }
  ba[whichChar] |= mask;
}
int PassCuts(DS_DATASET_T *pTable,long row,char *cuts) {
  /*    wt=whichTable    cuts="r0.gt.30.or.()"    ip=inParens    */
  /* Return values: PARSE_ERR FALSE TRUE */
  int ser,ip,len,pp; char copy[PSIZE],left[PSIZE],rite[PSIZE];
  /* if(row!=1) return FALSE; */
  if(*cuts=='\0') return TRUE;  /* no cuts, so return TRUE */
  DelWhiteSpace(cuts); /* October 9 1995 ToLowerCase(cuts); */ 
  strncpy(gCopy,cuts,PSIZE-2); strncpy(copy,cuts,PSIZE-2);
  if(!DelOutParens(cuts)) {
    Say1("Parse error in dsuDoCuts().");
    return PARSE_ERR; /* BBB This was TRUE as from evl. */
  }
  ip=FALSE; len=strlen(cuts);
  for(pp=0;pp<len;pp++) {
    if(cuts[pp]=='(') ip++; if(cuts[pp]==')') ip--;
    if(!ip&&AtAnd(cuts,pp)) {
      strcpy(left,cuts); left[pp]='\0';
      if(PassCuts(pTable,row,left)==TRUE) {
        strcpy(rite,cuts+5+pp);
        if(PassCuts(pTable,row,rite)==TRUE) return TRUE;
        else return FALSE;
      }
      else return FALSE;
    }
    if(!ip&&AtOr(cuts,pp)) {
      strcpy(left,cuts); left[pp]='\0';
      if(PassCuts(pTable,row,left)==TRUE) return TRUE;
      strcpy(rite,cuts+4+pp);
      if(PassCuts(pTable,row,rite)==TRUE) return TRUE;
      return FALSE;
    }
  }
  ser=SingleExpressionResult(pTable,row,cuts);
  return ser;
} /* wt row cc */
void InsertChars(char *xx,int pos,char *yy) {
  char copy[COPY]; int ii,len,len2; len=strlen(xx); len2=strlen(yy);
  for(ii=len;ii>=pos;ii--) xx[ii+len2]=xx[ii];
  for(ii=len2-1;ii>=0;ii--) xx[ii+pos]=yy[ii];
}
void DeleteChars(char *xx,int pos,int hm) {
  int ii,len; len=strlen(xx);
  for(ii=pos;ii<=len;ii++) xx[ii]=xx[ii+hm];
}
void Sub(char *xx,char *ee,int hm,int where) {
  DeleteChars(xx,where,hm);
  InsertChars(xx,where,ee);
}
void ConvertFromCtoFortran(char *xx) {
  int len,ii,fo=7;
  while(fo) {
    fo=0; len=strlen(xx);
    for(ii=0;ii<len-1;ii++) {
      if(xx[ii]=='>'&&xx[ii+1]=='=') { fo=7; Sub(xx,".ge.",2,ii); break; }
      if(xx[ii]=='<'&&xx[ii+1]=='=') { fo=7; Sub(xx,".le.",2,ii); break; }
      if(xx[ii]=='='&&xx[ii+1]=='=') { fo=7; Sub(xx,".eq.",2,ii); break; }
      if(xx[ii]=='/'&&xx[ii+1]=='=') { fo=7; Sub(xx,".ne.",2,ii); break; }
      if(xx[ii]=='!'&&xx[ii+1]=='=') { fo=7; Sub(xx,".ne.",2,ii); break; }
      if(xx[ii]=='<'               ) { fo=7; Sub(xx,".lt.",1,ii); break; }
      if(xx[ii]=='>'               ) { fo=7; Sub(xx,".gt.",1,ii); break; }
    }
  }
}
#define SHOW 35
myBool dsuDoCuts(size_t nBytes,char *ba,char *cuts,DS_DATASET_T *pTable) {
  size_t numRows; long ii; char copy[COPY],litCopy[SHOW+5]; /* 5 for "..." */
  strncpy(copy,cuts,COPY-2); ConvertFromCtoFortran(copy);
  if(strlen(copy)>COPY-4) { Say1("Cuts string too big."); return FALSE; }
  gAlreadyDidIt=FALSE;
  strncpy(litCopy,cuts,SHOW+1); litCopy[SHOW]='\0';
  if(strlen(cuts)>SHOW) strcat(litCopy,"...");
  Progress(-5,(int)numRows,"Cuts Progress                       ",litCopy);
  if(sizeof(char)!=1) {
    /* If you get this error message, you may have to adjust BITSPERCHAR */
    Say1("This is dsuDoCuts(). I need 8 bit chars."); return FALSE;
  }
  dsTableRowCount(&numRows,pTable);
  for(ii=nBytes-1;ii>=0;ii--) ba[ii]=0;
  gTableError=FALSE;
  for(ii=0;ii<numRows;ii++) {
    if(ii%150==0) Progress(ii,(int)numRows,NULL,NULL);
    if(gTableError) { Say1("Error 66d in dsuDoCuts()."); return FALSE; }
    switch(PassCuts(pTable,ii,copy)) {
      case TRUE: SetThisRow(ii,ba); break;
      case FALSE: break;
      case PARSE_ERR: Say1("Parse error on your cuts."); return FALSE; break;
      default: Say1("Error 66c in dsuDoCuts()."); return FALSE;
    }
  }
  Progress(-10,(int)numRows,NULL,NULL);
  return TRUE;
}
myBool dsuRowPassedCuts(char *ba,long row) {
  long whichChar; int whichBit; char mask;
  whichBit=row%BITSPERCHAR; whichChar=row/BITSPERCHAR;
  switch(whichBit) {
    case 0: mask=1; break;
    case 1: mask=2; break;
    case 2: mask=4; break;
    case 3: mask=8; break;
    case 4: mask=16; break;
    case 5: mask=32; break;
    case 6: mask=64; break;
    case 7: mask=128; break;
    default: mask=0;
  }
  if((ba[whichChar] & mask)) return TRUE;
  return FALSE;
}
