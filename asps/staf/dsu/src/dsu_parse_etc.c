/***********************************************************  TYPEDEFS  **/

/***********************************************************  INCLUDES  **/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "dstype.h"
#include "dsxdr.h"
#include "dscuts.h"
/***********************************************************  DEFINES  **/
#define TOLERANCE 0.0001
#define ERR_1 "The number typed in the subscript box is too small.\nMin=1."
#define ERR_2 "The number typed in the subscript box\nis too big.\nMax=%d"
#define ERR_3 "Subscript for column array type is out of range in cuts."
#define ERR_4 "Subscript for column array type is out of range."
#define PP printf(
#define PSIZE 2200
#define COPY 500
#define BITSPERCHAR 8
#define PARSE_ERR         -1
#ifndef FALSE
#define FALSE              0
#endif
#ifndef TRUE
#define TRUE               1
#endif
#define OPERATOR_TYPE_PE   0
#define OPERATOR_TYPE_GT   1
#define OPERATOR_TYPE_LT   2
#define OPERATOR_TYPE_GE   3
#define OPERATOR_TYPE_LE   4
#define OPERATOR_TYPE_EQ   5
/***********************************************************  GLOBALS  **/
int dsu_gHaveAnInt,dsu_gRow;
char dsu_gToken[80];
char *dsu_gExpr;
DS_DATASET_T *dsu_gTblPtr;
char dsu_gStr[DSU_SIZE_OF_GSTR];
int dsu_gTableError,dsu_gAlreadyDidIt;
char dsu_gCopy[PSIZE];
int dsu_gDone;
/***********************************************************  DEFINES  **/
#define GRAPHICS 0
#define TEXT 1
#define PROGRESS 2
/***********************************************************  PROTOTYPES **/
void dsuLevel2(float *answer);
int Equal(int demandInteger,float v1,float v2);
/***********************************************************  FUNCTIONS  **/
void Say1(char *x) {
  if(!dsu_gAlreadyDidIt) { dsu_gAlreadyDidIt=TRUE; printf("%s\n",x); }
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
float dsuVal(char *cc) {
  size_t sizetRow,colNum;
  int dataType; float rv,fv; long iv; int sub;
  if(HasLetters(cc)) {
    sizetRow=dsu_gRow;
    sub=StripSubAndRetItsValue(cc);
    if(!dsFindColumn(&colNum,dsu_gTblPtr,cc)) {
      PP"Fatal error: can't find column \"%s\".\n",cc);
      dsu_gDone=7; return 0.0;
    }
    if(!TableValue(&dataType,dsu_gStr,&fv,&iv,
              sizetRow,colNum,dsu_gTblPtr,sub)) {
      dsu_gTableError=TRUE; return 0.0;
    }
    if(dataType==DSU_FLOAT) rv=fv;
    else if(dataType==DSU_INTEGER) { dsu_gHaveAnInt=TRUE; rv=iv; }
    else if(dataType==DSU_STRING) {
      PP"Fatal error:  char string used in cuts.\n");
      dsu_gDone=7; return 0.0;
    }
    /* PP"Val rets %f.  dataType=%d\n------------------\n",rv,dataType); */
    return rv;
  } else return(atof(cc));
}
#define DSU_DELIMITERS "+-*/()"
int dsuIsDelim(char x) {
  if(strchr(DSU_DELIMITERS,*dsu_gExpr)) return 7;
  return 0;
}
void GetToken(void) {
  char *tmp=dsu_gToken;
  *tmp=0;
  if(!*dsu_gExpr) return; /* end of expression */
  while(isspace(*dsu_gExpr)) ++dsu_gExpr; /* skip white space */
  if(dsuIsDelim(*dsu_gExpr)) {
    *tmp++=*dsu_gExpr++;
  } else {                   /* else if(isalpha(*dsu_gExpr)) old stuff */
    while(!dsuIsDelim(*dsu_gExpr)) *tmp++=*dsu_gExpr++;
  }
  *tmp=0;  /* End the found-token string with a zero. */
}
void dsuLevel5(float *answer) { /* Parentheses. */
  if(*dsu_gToken=='(') {
    GetToken();
    dsuLevel2(answer); /* Here's the recursion. */
    if(*dsu_gToken!=')') {
      PP"UNBALANCED PARENTHESES.%c\n",7); *answer=0; return;
    }
    GetToken(); /* Throw away the final ) without checking for it. */
  } else {
    *answer=dsuVal(dsu_gToken); /* Both table values and constants. */
    GetToken();
  }
}
void dsuLevel4(float *answer) { /* Unary + or - */
  register char op;
  if((op=*dsu_gToken)=='+' || op=='-') {
    GetToken();
  }
  dsuLevel5(answer);
  if(op=='-') *answer = -(*answer);
}
void dsuLevel3(float *answer) { /* Multiplication or division. */
  register char op; float temp; 
  dsu_gHaveAnInt=FALSE;
  dsuLevel4(answer);
  while((op=*dsu_gToken)=='*' || op=='/') {
    GetToken(); dsuLevel4(&temp);
    if(op=='*') *answer=*answer*temp;
    else if(op=='/') {
      if(temp==0.0) *answer=1e15;
      else {
        if(!dsu_gHaveAnInt) *answer=*answer/temp;
        else *answer=((int)*answer)/((int)temp);
      }
    }
  }
}
void dsuLevel2(float *answer) { /* Addition and subtraction. */
  register char op; float temp; 
  dsuLevel3(answer);
  while((op=*dsu_gToken)=='+' || op=='-') {
    GetToken(); dsuLevel3(&temp);
    if(op=='+') *answer=*answer+temp;
    else if(op=='-') *answer=*answer-temp;
  }
}
float dsuLevel1(char *expr,DS_DATASET_T *pTable,long row) {
                 /* This function is the entry point for parsing recursion. */
  float answer;
  dsu_gTblPtr=pTable; /* Don't carry this thru all the parsing recursion. */
  dsu_gRow=row;       /* Don't carry this thru all the parsing recursion. */
  dsu_gExpr=expr; /* Global dsu_gExpr is used in         parsing recursion. */
  GetToken();     /* Uses dsu_gExpr and fills dsu_gToken. */

  /* globals: dsu_gExpr, dsu_gToken, dsu_gRow, dsu_gTblPtr */
  dsuLevel2(&answer);
  return answer;
}
float dsu_Val1(char *cc,int loc,DS_DATASET_T *pTable,long row) {
  char dd[PSIZE]; strcpy(dd,cc); dd[loc]='\0';
  return dsuLevel1(dd,pTable,row);
}
float dsu_Val2(char *cc,int loc,DS_DATASET_T *pTable,long row) {
  char dd[PSIZE]; strcpy(dd,cc+loc+4);
  return dsuLevel1(dd,pTable,row);
}
int ProbablyInteger(float x) {
  int ix; float x2;
  if(x<0) x=-x;
  ix=x+0.5;
  x2=ix;
  if(Equal(0,x,x2)) return TRUE;
  return FALSE;
}
int Equal(int demandInt,float v1,float v2) {
  /* Approx int equality tests.  HACK */
  float rat;
  if(v1==v2) return TRUE;
  if(v1==-v2) return FALSE;
  if( !demandInt|| (ProbablyInteger(v1)&&ProbablyInteger(v2)) ) {
    rat=(v1-v2)/(v1+v2);
    if(rat>-TOLERANCE&&rat<TOLERANCE) return TRUE;
  }
  return FALSE;
}
int SingleExpressionResult(DS_DATASET_T *pTable,long row,char *cuts) {
  int location,type;  /* type tells whether .gt., .lt., etc. */
  float v1,v2;
  GetLocationAndTypeOfOperator(cuts,&location,&type);
  v1=dsu_Val1(cuts,location,pTable,row);
  v2=dsu_Val2(cuts,location,pTable,row);
#ifdef DEBUG
  /* PP"type=%d (le%d ge%d gt%d lt%d eq%d), val1=%f val2=%f\n",
  ** type, OPERATOR_TYPE_LE, OPERATOR_TYPE_GE, OPERATOR_TYPE_GT,
  ** OPERATOR_TYPE_LT, OPERATOR_TYPE_EQ,v1,v2); */
#endif
  switch(type) {
    case OPERATOR_TYPE_LE: if(Equal(7,v1,v2)) return TRUE; return(v1<v2);
    case OPERATOR_TYPE_GE: if(Equal(7,v1,v2)) return TRUE; return(v1>v2);
    case OPERATOR_TYPE_GT: return(v1>v2);
    case OPERATOR_TYPE_LT: return(v1<v2);
    case OPERATOR_TYPE_EQ: return Equal(7,v1,v2);
    case OPERATOR_TYPE_PE: Say1("Parse error on cuts.");  return PARSE_ERR;
    default: PP"fatal error in module evl_vs\n");
  }

  return FALSE;
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
  if(pcnt==0) 
     return TRUE;

  return FALSE;   
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
  strncpy(dsu_gCopy,cuts,PSIZE-2); strncpy(copy,cuts,PSIZE-2); 
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
  int ii,len,len2; len=strlen(xx); len2=strlen(yy);
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

void
dsu_Progress(int a,int total,char *junk1,char *junk2) {
  printf("        %3d percent finished\n",(100*a)/total);
}

#define SHOW 35
int dsuDoCuts(size_t nBytes,char *ba,char *cuts,DS_DATASET_T *pTable) {
  size_t numRows; long ii; char copy[COPY],litCopy[SHOW+5]; /* 5 for "..." */
  strncpy(copy,cuts,COPY-2); ConvertFromCtoFortran(copy); 
  if(strlen(copy)>COPY-4) { Say1("Cuts string too big."); return FALSE; }
  dsu_gAlreadyDidIt=FALSE;
  strncpy(litCopy,cuts,SHOW+1); litCopy[SHOW]='\0'; 
  if(strlen(cuts)>SHOW) strcat(litCopy,"...");
  if(sizeof(char)!=1) {
    /* If you get this error message, you may have to adjust BITSPERCHAR */
    Say1("This is dsuDoCuts(). I need 8 bit chars."); return FALSE;
  }
  dsTableRowCount(&numRows,pTable);
  for(ii=nBytes-1;ii>=0;ii--) ba[ii]=0;
  dsu_gTableError=FALSE;
  for(ii=0;ii<numRows;ii++) {
    if(ii%30000==0) dsu_Progress(ii,(int)numRows,NULL,NULL);
    if(dsu_gTableError) { Say1("Error 66d in dsuDoCuts()."); return FALSE; }
    switch(PassCuts(pTable,ii,copy)) {
      case TRUE: SetThisRow(ii,ba); break;
      case FALSE: break;
      case PARSE_ERR: Say1("Parse error on your cuts."); return FALSE; break;
      default: Say1("Error 66c in dsuDoCuts()."); return FALSE;
    }
  }
  return TRUE;
}
int dsuRowPassedCuts(char *ba,long row) {
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
void dsuTableValErr(int x) {
  printf("%cError %d in dsu.\n",7,x);
}
int dsuCutsArray(
  DS_DATASET_T *dsPtr, size_t colNum,int tentSubscript,
  int *off, size_t *dim, size_t *array_size_t, const char **typeName) {
  /* see comment uu4 */
  if(!dsColumnTypeName(typeName,dsPtr,colNum))          dsuTableValErr( 17);
  if(!dsColumnDimCount(dim,dsPtr,colNum))               dsuTableValErr( 18);
  if(!dsColumnDimensions(array_size_t,dsPtr,colNum))    dsuTableValErr( 19);
  if(*dim>0) { 
    *off=tentSubscript; if(*off<0||*off>=*array_size_t) dsuTableValErr( 20); 
  } else *off=0;
  return TRUE;
}
int TableValue(int *dType,char *uu,float *fv,long *iv,size_t row,
  size_t colNum,DS_DATASET_T *tp,int subscript) {
  /* Either fv or iv is filled in.  Caller knows which from dType.  This
  ** function is the workhorse for getting numbers from the current table. */
  int ii,off; size_t colSize,rowSize;
  char *pData;const char *tn;
  size_t dim;             /* 0 for x, 1 for x[], and 2 for x[][]. */
  size_t arraysize;       /* x[arraysize] */
  if(!dsuCutsArray(tp,colNum,subscript,&off,&dim,&arraysize,&tn)) {
    dsuTableValErr( 21); return 0;
  }
  if(!dsCellAddress(&pData,tp,row,colNum)) {
    /* 961003 dsuTableValErr( 22); */ *fv=0.0; return FALSE;
  }
  if(!strcmp(tn,  "long")) {
    *iv=*((  long*)(pData+off*sizeof(long))); *dType=DSU_INTEGER;

  } else if(!strcmp(tn,   "short")) {
    *iv=*((   short*)(pData+off*sizeof(short))); *dType=DSU_INTEGER;
  } else if(!strcmp(tn,   "unsigned short")) {
    *iv=*((unsigned short*)(pData+off*sizeof(unsigned short))); 
    *dType=DSU_INTEGER;
  } else if(!strcmp(tn,   "unsigned long")) {
    *iv=*((unsigned long*)(pData+off*sizeof(unsigned long))); 
    *dType=DSU_INTEGER;
  } else if(!strcmp(tn,"octet")) {
    *iv=*((unsigned char*)(pData+off)); *dType=DSU_HEX;

  } else if(!strcmp(tn,   "int")) {
    *iv=*((   int*)(pData+off*sizeof(int))); *dType=DSU_INTEGER;
  } else if(!strcmp(tn, "float")) {
    *fv=*(( float*)(pData+off*sizeof(float))); *dType=DSU_FLOAT;
  } else if(!strcmp(tn,"double")) {
    *fv=*((double*)(pData+off*sizeof(double))); *dType=DSU_FLOAT;
  } else if(!strcmp(tn,"char")) {
    if(!dsColumnSize(&colSize,tp,colNum)) {dsuTableValErr(229); return 0;}
    if(!dsTableRowSize(&rowSize,tp)) {dsuTableValErr(223); return 0;}
    /* use row size for char strings, but sizeof() for others July 25 1995 */

    /* Aug 27 1995  off should never be anything but zero for char arrays,
    ** since we don't access the chars individually. */
    if(off!=0) {dsuTableValErr(772); return 0;}

    /*           */ strncpy(uu,(char*)(pData+off*rowSize),98); 
    if(colSize<98) uu[colSize]='\0';
    for(ii=strlen(uu)-1;ii>=0;ii--) { if(uu[ii]<32||uu[ii]>126) uu[ii]=' '; }
    for(ii=strlen(uu)-1;ii>=0;ii--) { if(uu[ii]!=' ') break; uu[ii]='\0'; }
    *dType=DSU_STRING;
  } else {
    PP"This table contains a data type (%s)\n",tn);
    PP"which I do not currently support.  Fatal error.\n");
    return FALSE;
  }
  /* PP"tABLEvALUE rets iflt=%d fv=%f, iv=%d\n",*dType,*fv,*iv); */
  return TRUE;
}
