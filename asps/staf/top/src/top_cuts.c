#include <stdio.h>
#include "dstype.h"
#include "dsxdr.h"
typedef int myBool;
#include "dscuts.h"

#define PP printf(
int gDone;
char gStr[100];

void CutsInit(void) {
  printf("Initializing cuts.\n"); gDone=0;
}
void Progress(int jj,int junk,char *junk2,char *junk3) {
  if(jj<0) return;
  if(jj<1000||jj%3300==0) 
      printf("       I:         %d \n",jj); /* modulus=150n */
}
void Say(char *x) {
  printf("%s\n",x);
}
void ErrTop(int x) {
  PP"Error number %d in function TableValue() of file %s.\n",__FILE__);
}
myBool TopArray(
  DS_DATASET_T *dsPtr, size_t colNum,int tentSubscript,
  int *off, size_t *dim, size_t *array_size_t, char **typeName) {
  /* see comment uu4 */
  if(!dsColumnTypeName(typeName,dsPtr,colNum))          ErrTop( 17);
  if(!dsColumnDimCount(dim,dsPtr,colNum))               ErrTop( 18);
  if(!dsColumnDimensions(array_size_t,dsPtr,colNum))    ErrTop( 19);
  if(*dim>0) { *off=tentSubscript; if(*off<0||*off>=*array_size_t) ErrTop( 20); }
  else *off=0;
  return TRUE;
}
myBool TableValue(int *dType,char *uu,float *fv,long *iv,size_t row,
  size_t colNum,DS_DATASET_T *tp,int subscript) {
  /* Either fv or iv is filled in.  Caller knows which from dType.  This
  ** function is the workhorse for getting numbers from the current table. */
  int ii,off; size_t colSize,rowSize;
  char *pData,*tn;
  size_t dim;             /* 0 for x, 1 for x[], and 2 for x[][]. */
  size_t arraysize;       /* x[arraysize] */
  if(!TopArray(tp,colNum,subscript,&off,&dim,&arraysize,&tn)) {
    ErrTop( 21); return 0;
  }
  if(!dsCellAddress(&pData,tp,row,colNum)) {
    /* 961003 ErrTop( 22); */ *fv=0.0; return FALSE;
  }
  if(!strcmp(tn,  "long")) {
    *iv=*((  long*)(pData+off*sizeof(long))); *dType=INTEGER;

  } else if(!strcmp(tn,   "short")) {
    *iv=*((   short*)(pData+off*sizeof(short))); *dType=INTEGER;
  } else if(!strcmp(tn,   "unsigned short")) {
    *iv=*((unsigned short*)(pData+off*sizeof(unsigned short))); *dType=INTEGER;
  } else if(!strcmp(tn,   "unsigned long")) {
    *iv=*((unsigned long*)(pData+off*sizeof(unsigned long))); *dType=INTEGER;
  } else if(!strcmp(tn,"octet")) {
    *iv=*((unsigned char*)(pData+off)); *dType=HEX;

  } else if(!strcmp(tn,   "int")) {
    *iv=*((   int*)(pData+off*sizeof(int))); *dType=INTEGER;
  } else if(!strcmp(tn, "float")) {
    *fv=*(( float*)(pData+off*sizeof(float))); *dType=FLOAT;
  } else if(!strcmp(tn,"double")) {
    *fv=*((double*)(pData+off*sizeof(double))); *dType=FLOAT;
  } else if(!strcmp(tn,"char")) {
    if(!dsColumnSize(&colSize,tp,colNum)) {ErrTop(229); return 0;}
    if(!dsTableRowSize(&rowSize,tp)) {ErrTop(223); return 0;}
    /* use row size for char strings, but sizeof() for others July 25 1995 */

    /* Aug 27 1995  off should never be anything but zero for char arrays,
    ** since we don't access the chars individually. */
    if(off!=0) {ErrTop(772); return 0;}

    /* gStr[100] */ strncpy(uu,(char*)(pData+off*rowSize),98);
    if(colSize<98) uu[colSize]='\0';
    for(ii=strlen(uu)-1;ii>=0;ii--) { if(uu[ii]<32||uu[ii]>126) uu[ii]=' '; }
    for(ii=strlen(uu)-1;ii>=0;ii--) { if(uu[ii]!=' ') break; uu[ii]='\0'; }
    *dType=STRING;
  } else {
    PP"This table contains a data type (%s)\n",tn);
    PP"which the browser does not currently support.  Fatal error.\n");
    return FALSE;
  }
  /* PP"tABLEvALUE rets iflt=%d fv=%f, iv=%d\n",*dType,*fv,*iv); */
  return TRUE;
}
