/* Begun May 1 1995, Herb Ward.  ward@physics.utexas.edu
 Here are instructions for people who want to use my table cuts mechanism
 under dsl.
 At the risk of looking difficult, these notes are detailed.
 This file (and its companion cuts.h, which you should #include in your
 files where you call anything from this file, contain
 software for doing table cuts under dsl for STAR.  You must link
 to STAR's dsl and dsu libraries.
 There are two functions (DoCuts() and RowPassedCuts()) that you call.
 Here is how to use them.
 1.  Declarations:
        char ba[NBYTES];       (ba means Boolean Array)
        size_t rowIndex,nBytes;
        DS_DATASET_T *pTable;
 2.  Determine the number of rows in your table.  Divide this number by 8,
     round up to the next integer.  Check that your result <= NBYTES.
     (I use every bit (to conserve memory) instead of all eight bits to
     store one true/false.)
 3.  Make a cuts string [eg, "(id.ge.200.and.id.le.299).or.(invpt.le.5.23)"].
     The identifiers (eg "invpt") are column names.
     Use square brackets for columns whose data types are arrays [eg,
     "cov[5].gt.6.12"].
 4.  Use dsl to make a pointer to the table named "pTable".
 5.  Call the first function
       myBool DoCuts(char *errMess,size_t NBYTES,
        char *ba,char *cuts,DS_DATASET_T *pTable);
     It returns FALSE (0) in case of error, TRUE (1) otherwise.
     Error messages are written in errMess, less than 80 bytes.
     It fills in the array ba, the primary output.
 6.  To determine whether a given row (row number rowIndex) passed the cuts,
     call the second function:
        myBool RowPassedCuts(char *ba,size_t rowIndex);
     Here, returning FALSE does not indicate an error condition, but rather
     simply means that the row in question failed during the call to DoCuts.
     NB:  set rowIndex according to C conventions, and not FORTRAN conventions
     (ie, the first row has rowIndex=0, NOT rowIndex=1).
     NB:  you CANNOT use the construction if(ba[rowIndex]), you MUST use
     if(RowPassedCuts(ba,rowIndex)).
 7.  You must provide a function void Say(char *) for error messages from
     this file.  Say() must add a newline, if required.  A simple Say() would
     be (this has to be in YOUR code for ME to call):
     #include <stdio.h>
     void Say(char *errMessageFromCuts) {
       printf("Error message from cuts:\n%s\n",errMessageFromCuts);
     }
 8.  You must provide a second function named Err.  You might use the
     following code for your Err() (this has to be (this has to be in
     YOUR code for ME to call):
         #include <stdio.h>
         void Err(int xx) {
            printf("Fatal error number %d during cuts.\n",xx);
         }
*/
/***********************************************************  TYPEDEFS  **/
typedef int myBool;
/***********************************************************  INCLUDES  **/
#include <stdio.h>
#include <math.h>
#include <strings.h>
#include <stdlib.h>
#include "dstype.h"
#include "dsxdr.h"
#include "cuts.h"
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
  DelWhiteSpace(cuts); ToLowerCase(cuts);
  strncpy(gCopy,cuts,PSIZE-2); strncpy(copy,cuts,PSIZE-2);
  if(!DelOutParens(cuts)) {
    Say1("Parse error in DoCuts().");
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
myBool DoCuts(size_t nBytes,char *ba,char *cuts,DS_DATASET_T *pTable) {
  size_t numRows; long ii; char copy[COPY];
  strncpy(copy,cuts,COPY-2); ConvertFromCtoFortran(copy);
  if(strlen(copy)>COPY-4) { Say1("Cuts string too big."); return FALSE; }
  gAlreadyDidIt=FALSE;
  Progress(0);
  if(sizeof(char)!=1) {
    /* If you get this error message, you may have to adjust BITSPERCHAR */
    Say1("This is DoCuts(). I need 8 bit chars."); return FALSE;
  }
  dsTableRowCount(&numRows,pTable);
  for(ii=nBytes-1;ii>=0;ii--) ba[ii]=0;
  gTableError=FALSE;
  for(ii=0;ii<numRows;ii++) {
    if(ii%300==0) Progress(ii);
    if(gTableError) { Say1("Error 66d in DoCuts()."); return FALSE; }
    switch(PassCuts(pTable,ii,copy)) {
      case TRUE: SetThisRow(ii,ba); break;
      case FALSE: break;
      case PARSE_ERR: Say1("Parse error on your cuts."); return FALSE; break;
      default: Say1("Error 66c in DoCuts()."); return FALSE;
    }
  }
  return TRUE;
}
myBool RowPassedCuts(char *ba,long row) {
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
