/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        topClasses.C
**:DESCRIPTION: TOP-Table OPerators ASP C++ code
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     23oct96-v001a-cet- make it work
**:HISTORY:     13jun96-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

//:----------------------------------------------- INCLUDES           --
#include "asuAlloc.h"
#include "topClasses.hh"
#include "tdmClasses.hh"
#include "top_utils.h"

extern "C" int TableValue(int *dataType,char *xx,float *fv,long *iv,
  size_t row, size_t colNum,DS_DATASET_T *tp,int subscript);
extern "C" void CutsInit(void);
extern "C" int dsuDoCuts(size_t bytes, char *ba, char *cut
		,DS_DATASET_T *pTab);
extern "C" int dsuRowPassedCuts(char *ba,long row);
extern "C" int IsValidCutFunc(char*);
extern "C" int topFastjoin(DS_DATASET_T *pJoinTable
	, DS_DATASET_T *pTableOne
        , DS_DATASET_T *pTableTwo, char *aliases
        , char *joinList, char *projectList);
//:----------------------------------------------- MACROS             --
#define PP printf(
//:----------------------------------------------- PROTOTYPES         --
extern "C" const char* id2name(const char* base,long id);

//:#####################################################################
//:=============================================== CLASS              ==
//: topProject

//:----------------------------------------------- CTORS & DTOR       --
topProject:: topProject()
		: socObject("NULL","topProject") {
   myPtr = (SOC_PTR_T)this;
   mySelectSpec = NULL;
}

//:---------------------------------
topProject:: topProject(const char * name, const char * spec)
		: socObject(name, "topProject") {
   myPtr = (SOC_PTR_T)this;
   if( isValidSelectSpec(spec) ){
      mySelectSpec = (char*)MALLOC(strlen(spec) +1);
      strcpy(mySelectSpec, spec);
   }
   else {
      mySelectSpec = NULL;
   }
}

//:---------------------------------
topProject:: ~topProject(){
   if(mySelectSpec) FREE(mySelectSpec);
}

//:----------------------------------------------- ATTRIBUTES         --
char* topProject:: selectionSpecification() {
   char* c=NULL;
   if(mySelectSpec) {
     c = (char*)MALLOC(strlen(mySelectSpec) +1);
     strcpy(c,mySelectSpec);
   } else {
     char *herb22Feb98 = "invalidSelectSpec";
     c = (char*)MALLOC(strlen(herb22Feb98) +1);
     strcpy(c,herb22Feb98);
   }
   return c;
}

//----------------------------------
void topProject:: selectionSpecification(const char* spec) {
   if( isValidSelectSpec(spec) ){
      if(mySelectSpec) FREE(mySelectSpec);
      mySelectSpec = (char*)MALLOC(strlen(spec) +1);
      strcpy(mySelectSpec,spec);
   }
}

//----------------------------------
// OVERRIDE socObject::listing()
char * topProject::  listing () {
   char* c = socObject::listing();
   char* cc = NULL;
   char* s = selectionSpecification();
   cc = (char*)MALLOC(79+100);
   sprintf(cc,"%s %-32s",c,s);
   FREE(c);
   FREE(s);
   return cc;
}

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T topProject:: project(tdmTable * table1, tdmTable *& table2) {
   DS_DATASET_T *pTbl1=table1->dslPointer(); //HACK -collocated only!!!
   DS_DATASET_T *pTbl2=NULL;
   if( NULL == table2 ){
      table2 = pTarget(table1, NULL);
   }
   if(!table2) EML_ERROR(OUTPUT_TABLE_NOT_READY);
   pTbl2=table2->dslPointer();
   if(!mySelectSpec) EML_ERROR(INVALID_SELECTION_SPEC);
   if( !dsProjectTable(pTbl2,pTbl1,mySelectSpec) ){
      EML_ERROR(PROJECTION_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
tdmTable* topProject:: pTarget(tdmTable * table1, const char * name) {
   DS_DATASET_T *pTbl1=table1->dslPointer();
   tdmTable * table2=NULL;
   DS_DATASET_T *pTbl2=NULL;
   const char *n=NULL;
   if( NULL == name ){
//   n=id2name("projection",soc->nextIDref());
     n=id2name("projection",111); // HACK 
   }
   else {
     n=name;
   }
   if(!mySelectSpec) EML_ERROR(INVALID_SELECTION_SPEC);
   if( !dsTargetTable(&pTbl2, n, n, pTbl1, NULL, NULL, 
	     mySelectSpec) ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
// table2 = new tdmTable(pTbl2);		// HACK !!!
   const char *n2=NULL;
   const char *s2=NULL;
   if( !dsTableName(&n2,pTbl2) 
   ||  !dsTableTypeSpecifier(&s2,pTbl2) 
   ){
      EML_ERROR(CANT_CREATE_TABLE);
      FREE(pTbl2); /*fix memory leak -akio*/
   }
   if( NULL == (table2 = tdm->newTable(n2,s2,0)) ){
      EML_ERROR(CANT_CREATE_TABLE);
      FREE(pTbl2); /*fix memory leak -akio*/
   }
   FREE(pTbl2); /*fix memory leak -akio*/
   return table2;
}

//----------------------------------
STAFCV_T topProject:: reset() {
   if(mySelectSpec) FREE(mySelectSpec);
   mySelectSpec = NULL;
   EML_SUCCESS(STAFCV_OK);
}

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: topCut

//:----------------------------------------------- CTORS & DTOR       --
topCut:: topCut()
		: socObject("NULL","topCut") {
   myPtr = (SOC_PTR_T)this;
   myCutFunction = NULL;
}

//:---------------------------------
topCut:: topCut(const char * name, const char * func)
		: socObject(name, "topCut") {
   myPtr = (SOC_PTR_T)this;
   int bad=0;
   if(strlen(func)<3) bad=1;
   if( !bad ){
      myCutFunction = (char*)MALLOC(strlen(func) +1);
      strcpy(myCutFunction, func);
   }
   else {
      printf("The cut function you gave me is flawed.  Try again.\n");
      myCutFunction = NULL;
   }
}

//:---------------------------------
topCut:: ~topCut(){
   FREE(myCutFunction);
}

//:----------------------------------------------- ATTRIBUTES         --
char* topCut:: cutFunction() {
   char* c=NULL;
   c = (char*)MALLOC(strlen(myCutFunction) +1);
   strcpy(c,myCutFunction);
   return c;
}

//----------------------------------
// OVERRIDE socObject::listing()
char * topCut::  listing () {
   char* c = socObject::listing();
   return c;				// HACK - WORK!!!
}

//:----------------------------------------------- INTERFACE METHODS  --

STAFCV_T topCut:: DoCutTable(tdmTable *tbl,char *func,
    long *orig,long *percentPass) {
  size_t numBytesToTransfer,bytesPerRow,nbytes; long startRow,row;
  char *bottomNewTbl,*beginningOfTable,*copyThis;
  long rowCnt=0,colCnt; void *mask;
  DS_DATASET_T* dsPtr;
  colCnt=tbl->columnCount();
  *orig=tbl->rowCount();

  dsPtr=tbl->dslPointer();
  if(!dsTableRowSize(&bytesPerRow,dsPtr)) 
      EML_ERROR(CANT_FIND_ROW_SIZE);
  if(!dsTableDataAddress(&beginningOfTable,dsPtr)) 
      EML_ERROR(CANT_FIND_DATA);
  bottomNewTbl=beginningOfTable;

  nbytes=(size_t)(((*orig)/8)+1); mask=MALLOC(nbytes);
  if(!mask) { printf("Could not allocate %d bytes.\n",nbytes); return 0; }

  CutsInit();
  if(!dsuDoCuts(nbytes,(char*)mask,(char*)func,dsPtr)) {
    printf("Failure, check your cuts string for syntax errors:\n");
    fputs(func,stdout); printf("\n");
    FREE(mask); //*VP-phenix* 
    return 0;
  }
  startRow=-10;
  for(row=0;row<*orig+1;row++) { /* The +1 is deliberate, to invoke the
                                 ** else clause during ending of the loop. */
    if(row<(*orig)&&dsuRowPassedCuts((char*)mask,row)) {
      if(row%1557==0) printf("Phase II: %ld %% complete.\n",(row*100)/(*orig));
      rowCnt++; if(startRow<0) startRow=row;
    } else {
      if(startRow>=0) {
        numBytesToTransfer=(size_t)((row-startRow)*bytesPerRow);
        copyThis=(char*)(beginningOfTable+bytesPerRow*startRow);
        memcpy((void*)bottomNewTbl,(void*)copyThis,numBytesToTransfer);
        startRow=-10;
        bottomNewTbl+=numBytesToTransfer;
      }
    }
  }
  printf("%ld rows passed the cuts.\n",rowCnt);
  *percentPass=(long)((100.0*rowCnt)/(*orig)+0.5);
  tbl->rowCount(rowCnt);
  FREE(mask); /*fix memory leak -akio*/
  return 7;
}
STAFCV_T topCut:: DoFilterTable(tdmTable *src,
    tdmTable *tgt,char *func,long *orig,
    long *percentPass) {
  size_t numBytesToTransfer,bytesPerRow,nbytes; long startRow,row;
  char *beginOfSrcTbl,*bottomNewTbl,*copyThis;
  long numberPass=0,colCnt; void *mask; int numMsg=0;
  DS_DATASET_T *dsPtr,*tgtPtr;
  colCnt=src->columnCount();
  *orig=src->rowCount();

  dsPtr=src->dslPointer(); // ONLY in a collocated process (CORBA)
  tgtPtr=tgt->dslPointer(); // ONLY in a collocated process (CORBA)
  if(!dsTableRowSize(&bytesPerRow,tgtPtr)) 
        EML_ERROR(CANT_FIND_ROW_SIZE);

  nbytes=(size_t)(((*orig)/8)+1); mask=MALLOC(nbytes);
  if(!mask) { printf("Could not allocate %d bytes.\n",nbytes); return 0; }

  CutsInit();
  if(!dsuDoCuts(nbytes,(char*)mask,(char*)func,dsPtr)) {
    printf("Failure, check your cuts string for syntax errors:\n");
    fputs(func,stdout); printf("\n");
    FREE(mask); //*VP-phenix*
    return 0;
  }
  startRow=-10;
  for(row=0;row<*orig;row++) { /* 1st pass, set rowcount new table*/
    if(dsuRowPassedCuts((char*)mask,row)) (numberPass)++;
  }
  tgt->maxRowCount(numberPass);
  tgt->rowCount((numberPass));
  if(!dsTableDataAddress(&beginOfSrcTbl,dsPtr)) 
       EML_ERROR(CANT_FIND_DATA);
  if(!dsTableDataAddress(&bottomNewTbl,tgtPtr)) 
      EML_ERROR(CANT_FIND_DATA);
  for(row=0;row<(*orig)+1;row++) { /* The <x+1 is deliberate, to invoke the
                                   ** else clause during ending of the loop. */
    if(row<(*orig)&&dsuRowPassedCuts((char*)mask,row)) {
      if(numMsg<5) {
        if(row%1557==0) {
          printf("Phase II: %ld %% complete.\n",(row*100)/(*orig)); numMsg++;
        }
      } else {
        if(row%55570==0) {
          printf("Phase II: %ld %% complete.\n",(row*100)/(*orig)); numMsg++;
        }
      }
      if(startRow<0) startRow=row;
    } else {
      if(startRow>=0) {
        numBytesToTransfer=(size_t)((row-startRow)*bytesPerRow);
        copyThis=(char*)(beginOfSrcTbl+bytesPerRow*startRow);
        memcpy((void*)bottomNewTbl,(void*)copyThis,numBytesToTransfer);
        startRow=-10;
        bottomNewTbl+=numBytesToTransfer;
      }
    }
  }
  printf("%ld rows passed the cuts.\n",numberPass);
  *percentPass=(long)((100.0*numberPass)/(*orig)+0.5);
  FREE(mask); /*fix memory leak -akio*/
  return 7;
}
STAFCV_T topCut:: filter(tdmTable * tab1, tdmTable * tab2) {
   long orig,percent;
   if( NULL == tab1 || NULL == tab2 ){
      EML_ERROR(INVALID_OBJECT);
   }
   /* DS_DATASET_T *pTbl1=tab1->dslPointer(); */
   /* DS_DATASET_T *pTbl2=tab2->dslPointer(); */
   tab2->maxRowCount(0);		/* FREE tab2 DATA MEMORY */
   if( !DoFilterTable(tab1,tab2,myCutFunction,&orig,&percent) ){
      EML_ERROR(FILTER_FAILURE);
   }
   printf("The original table had %ld rows.  %ld percent of these passed.\n",
   orig,percent);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topCut:: cut(tdmTable * table1) {
   long orig,percent;
   if( NULL == table1 ){
      EML_ERROR(INVALID_OBJECT);
   }
   // CET does not like this.  DS_DATASET_T *pTbl1=table1->dslPointer();
   if( !DoCutTable(table1,myCutFunction,&orig,&percent) ){
      EML_ERROR(CUT_FAILURE);
   }
   printf("The original table had %ld rows.  %ld percent of these passed.\n",
   orig,percent);
   EML_SUCCESS(STAFCV_OK);
}


//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: topSort

topSort:: topSort()
		: socObject("NULL","topSort") {
   myPtr = (SOC_PTR_T)this;
   myWhichColumn = NULL;
}

topSort:: topSort(const char * name, const char * whichCol) 
		: socObject(name,"topSort") {
   myPtr = (SOC_PTR_T)this;
   myWhichColumn = (char*)MALLOC(strlen(whichCol) +1);
   strcpy(myWhichColumn,whichCol);
}

topSort:: ~topSort(){
   FREE(myWhichColumn);
}

void topSort:: whichColumn(char *whichCol) {
  FREE(myWhichColumn);
  myWhichColumn = (char*)MALLOC(strlen(whichCol) +1);
  strcpy(myWhichColumn,whichCol);
}
char* topSort:: whichColumn() {
  char* c=NULL;
  c = (char*)MALLOC(strlen(myWhichColumn) +1);
  strcpy(c,myWhichColumn);
  return c;
}
void 
topSort::SwapRows(void *data, size_t rowsize,void *temp,
		  DS_DATASET_T* dsPtr,int rowNumberA,int rowNumberB) 
{
  // Just to hush pedantic compilers
  static void *pd = &dsPtr;

  void *row1,*row2;
  row1=(void*)((char*)data+(rowsize*rowNumberA));
  row2=(void*)((char*)data+(rowsize*rowNumberB));
  memcpy( temp , row1 , rowsize );
  memcpy( row1 , row2 , rowsize );
  memcpy( row2 , temp , rowsize );
}
float topSort::Value(int *errFlag,DS_DATASET_T* dsPtr,
      int rownumInt,size_t colNum) {
  float returnValue;
  int dataType;
  char junk[103];
  long intValue;
  float floatValue;
  size_t rownum=rownumInt;
  if(TableValue(&dataType,junk,&floatValue,&intValue,rownum,colNum,dsPtr,0)) {
    if(dataType==1) returnValue=intValue;
    else if(dataType==0) returnValue=floatValue;
    else { *errFlag=1; returnValue=0.0;  } /* string or hex */
  } else {
    *errFlag=2; returnValue=0.0;
  }
  return returnValue;
}
void topSort::top_qsort(void *data,size_t rowsize,void *temp,
    DS_DATASET_T* pp,int left,int rite,size_t colNum,int *errFlag) {
  register int ii,jj;
  ii=left; jj=rite;
  float comparator;
  comparator=Value(errFlag,pp,(left+rite)/2,colNum);
  do {
    while(Value(errFlag,pp,ii,colNum)<comparator&&ii<rite) ii++;
    while(Value(errFlag,pp,jj,colNum)>comparator&&jj>left) jj--;
    if(ii<=jj) {
      if(ii<jj) SwapRows(data,rowsize,temp,pp,ii,jj);
      ii++; jj--;
    }
  } while(ii<=jj);
  if(left<jj) {
    top_qsort(data,rowsize,temp,pp,left,jj,colNum,errFlag);
  }
  if(ii<rite) {
    top_qsort(data,rowsize,temp,pp,ii,rite,colNum,errFlag);
  }
}
STAFCV_T topFactory:: topOperator (tdmTable *table,char *colName,
    char *operation,char *valueString) {
  DS_DATASET_T* dsPtr=table->dslPointer();
  size_t irow,colNum,nrows; long ival; int operationType,dataType; 
  char *aa,junk[103];
  short shortX; unsigned short ushortX; long longX;
  unsigned long ulongX; float floatX; double doubleX;
  DS_TYPE_CODE_T colType;
  float newvalue,value,fval,cellValue;
  if(!strcmp(operation,"add")) operationType=0;
  else if(!strcmp(operation,"subtract")) operationType=1;
  else if(!strcmp(operation,"multiply")) operationType=2;
  else if(!strcmp(operation,"divide")) operationType=3;
  else {
    EML_CONTEXT("ERROR: I need 'add', 'subtract', 'multiply' or 'divide'.\n");
    EML_ERROR(INVALID_OPERATION);
  }
  value=atof(valueString);
  if(strstr(colName,"[")) EML_ERROR(VECTOR_COLS_NOT_SUPPORTED);
  if(!dsTableRowCount(&nrows,dsPtr)) EML_ERROR(TABLE_NOT_FOUND);
  if(!dsFindColumn(&colNum,dsPtr,colName)) EML_ERROR(COLUMN_NOT_FOUND);
  if(!dsColumnTypeCode(&colType,dsPtr,colNum)) EML_ERROR(CANT_GET_COL_TYPE);
  printf("Number rows = %d, column number = %d (count from zero).\n",
  nrows, colNum);
  switch(colType) {
    case DS_TYPE_SHORT:   aa=(char*)&shortX;  break;
    case DS_TYPE_U_SHORT: aa=(char*)&ushortX; break;
    case DS_TYPE_LONG:    aa=(char*)&longX;   break;
    case DS_TYPE_U_LONG:  aa=(char*)&ulongX;  break;
    case DS_TYPE_FLOAT:   aa=(char*)&floatX;  break;
    case DS_TYPE_DOUBLE:  aa=(char*)&doubleX; break;
    default: 
      EML_CONTEXT("ERROR: col data types char, octet, etc not supported.\n");
      EML_ERROR(UNSUPPORTED_COLUMN_DATA_TYPE);
  }
  for(irow=0;irow<nrows;irow++) {
    if(!TableValue(&dataType,junk,&fval,&ival,irow,colNum,dsPtr,0))
          EML_ERROR(CANT_FIND_VALUE);
    if(dataType==1) cellValue=ival;
    else if(dataType==0) cellValue=fval;
    else EML_ERROR(INVALID_COLUMN_DATA_TYPE);
    switch(operationType) {
      case 0: newvalue=cellValue+value; break;
      case 1: newvalue=cellValue-value; break;
      case 2: newvalue=cellValue*value; break;
      case 3: newvalue=cellValue/value; break;
      default: EML_ERROR(STAF_PROGRAMMERS_ERROR);
    }
    switch(colType) {
      case DS_TYPE_SHORT:   shortX=(short)newvalue; break;
      case DS_TYPE_U_SHORT: ushortX=(unsigned short)newvalue; break;
      case DS_TYPE_LONG:    longX=(long)newvalue; break;
      case DS_TYPE_U_LONG:  ulongX=(unsigned long)newvalue; break;
      case DS_TYPE_FLOAT:   floatX=(float)newvalue; break;
      case DS_TYPE_DOUBLE:  doubleX=(double)newvalue; break;
      default: 
        EML_CONTEXT("ERROR: col data types char, octet, etc not supported.\n");
        EML_ERROR(UNSUPPORTED_COLUMN_DATA_TYPE);
    }
    if(!dsPutCell(aa,dsPtr,irow,colNum)) EML_ERROR(PUTCELL_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}
/*
extern "C" int TableValue(int *dataType,char *xx,float *fv,long *iv,
  size_t row, size_t colNum,DS_DATASET_T *tp,int subscript);
*/
STAFCV_T topSort:: SortTheTable(tdmTable *table) {
  DS_DATASET_T* dsPtr = table->dslPointer();
  size_t nrows,rowsize,colNum;
  int errFlag;
  char *dataAddr;
  void *tmp;

  if(!dsTableRowCount(&nrows,dsPtr)) EML_ERROR(TABLE_NOT_FOUND);
  PP"number of rows is %d.\n",nrows);

  if(!dsTableRowSize(&rowsize,dsPtr)) EML_ERROR(CANT_FIND_ROW_SIZE);
  if(!dsTableDataAddress(&dataAddr,dsPtr)) EML_ERROR(CANT_FIND_DATA);
  if(!dsFindColumn(&colNum,dsPtr,myWhichColumn)) 
      EML_ERROR(CANT_FIND_COLUMN);

  tmp=MALLOC(rowsize);
  errFlag=0;
  top_qsort((void*)dataAddr,rowsize,tmp,dsPtr,0,nrows-1,colNum,&errFlag);
  FREE(tmp);
  if(errFlag) {
    if(errFlag==1) EML_ERROR(INVALID_COLUMN_DATA_TYPE);
    EML_ERROR(SORT_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}
STAFCV_T topSort:: sort(tdmTable *table) {
  if(!SortTheTable(table)) EML_ERROR(SORT_FAILURE);
  EML_SUCCESS(STAFCV_OK);
}
//:#####################################################################
//:=============================================== CLASS              ==
//: topJoin

//:----------------------------------------------- CTORS & DTOR       --
topJoin:: topJoin()
		: topProject()
		, socObject("NULL","topJoin") {
   myPtr = (SOC_PTR_T)this;
   mySelectSpec = NULL;
   myWhereClause = NULL;
}

//:---------------------------------
topJoin:: topJoin(const char * name, const char * spec
		, const char * clause)
		: topProject()
		, socObject(name, "topJoin") {
   myPtr = (SOC_PTR_T)this;
   if( isValidSelectSpec(spec) ){
      mySelectSpec = (char*)MALLOC(strlen(spec) +1);
      strcpy(mySelectSpec, spec);
   }
   else {
      mySelectSpec = NULL;
   }
   if( isValidWhereClause((char*)clause) ){
      myWhereClause = (char*)MALLOC(strlen(clause) +1);
      strcpy(myWhereClause, clause);
   }
   else {
      myWhereClause = NULL;
   }
}

//:---------------------------------
topJoin:: ~topJoin(){
   FREE(myWhereClause);
}

//:----------------------------------------------- ATTRIBUTES         --
char* topJoin:: whereClause() {
   char* c=NULL;
   c = (char*)MALLOC(strlen(myWhereClause) +1);
   strcpy(c,myWhereClause);
   return c;
}

//:---------------------------------
void topJoin:: whereClause(const char* clause) {
   if( isValidWhereClause((char*)clause) ){
      FREE(myWhereClause);
      myWhereClause = (char*)MALLOC(strlen(clause) +1);
      strcpy(myWhereClause,clause);
   }
}

//----------------------------------
// OVERRIDE socObject::listing()
char * topJoin::  listing () {
   char* c = socObject::listing();

   char* cc = NULL;
   char* s = selectionSpecification();
   char ss[16];
   strncpy(ss,s,15); 
   ss[15]=0; /* hjw 19Feb98 */
   char* w = whereClause();
   char ww[16];
   strncpy(ww,w,15); 
   ww[15]=0; /* hjw 19Feb98 */
   cc = (char*)MALLOC(79+100);
   sprintf(cc,"%s %s#%s",c,ss,ww);
   FREE(c); FREE(s); FREE(w);
   return cc;
}

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T topJoin:: fastjoin(tdmTable * table1, tdmTable * table2
		, tdmTable *& table3) {
   DS_DATASET_T *pTbl1=table1->dslPointer();
   DS_DATASET_T *pTbl2=table2->dslPointer();
   DS_DATASET_T *pTbl3=NULL;
   if( NULL == table3 ){
      table3 = jTarget(table1, table2, NULL);
      if( NULL == table3 ){
	 EML_PUSHERROR(DSL_ERROR);
	 EML_ERROR(FASTJOIN_FAILURE);
      }
   }
   pTbl3=table3->dslPointer();
   if(!mySelectSpec) EML_ERROR(INVALID_SELECTION_SPEC);
   if( !topFastjoin(pTbl3,pTbl1,pTbl2,NULL,myWhereClause,mySelectSpec) ){
      EML_PUSHERROR(DSL_ERROR);
      EML_ERROR(FASTJOIN_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);

}
//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T topJoin:: join(tdmTable * table1, tdmTable * table2
		, tdmTable *& table3) {
   DS_DATASET_T *pTbl1=table1->dslPointer();
   DS_DATASET_T *pTbl2=table2->dslPointer();
   DS_DATASET_T *pTbl3=NULL;
   if( NULL == table3 ){
      table3 = jTarget(table1, table2, NULL);
      if( NULL == table3 ){
	 EML_PUSHERROR(DSL_ERROR);
	 EML_ERROR(JOIN_FAILURE);
      }
   }
   pTbl3=table3->dslPointer();
   if(!mySelectSpec) EML_ERROR(INVALID_SELECTION_SPEC);
   if( !dsEquijoin(pTbl3,pTbl1,pTbl2,NULL,myWhereClause,mySelectSpec) ){
      EML_PUSHERROR(DSL_ERROR);
      EML_ERROR(JOIN_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);

}

//:---------------------------------
tdmTable * topJoin:: jTarget(tdmTable * table1, tdmTable * table2
		, const char * name) {
   DS_DATASET_T *pTbl1=table1->dslPointer();
   DS_DATASET_T *pTbl2=table2->dslPointer();
   char herb26June98;
   tdmTable * table3=NULL;
   DS_DATASET_T *pTbl3=NULL;
   const char *n=name;

   if( !name ){
//   n=id2name("projection",soc->nextIDref());
     n=id2name("join",111); // HACK 
   }
   if(!mySelectSpec) EML_ERROR(INVALID_SELECTION_SPEC);
   if( !dsTargetTable(&pTbl3, n, n, pTbl1, pTbl2, NULL, 
             mySelectSpec) ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
// table3 = new tdmTable(pTbl3);                // HACK !!!
   const char *n3=NULL;
   const char *s3=NULL;
   if( !dsTableName(&n3,pTbl3) 
   ||  !dsTableTypeSpecifier(&s3,pTbl3) 
   ){
      EML_ERROR(CANT_CREATE_TABLE);
      FREE(pTbl3); /*fix memory leak -akio*/
   }
   if( NULL == (table3 = tdm->newTable(n3,s3,0)) ){
      EML_ERROR(CANT_CREATE_TABLE);
      FREE(pTbl3); /*fix memory leak -akio*/
   }
   FREE(pTbl3); /*fix memory leak -akio*/
   return table3;
}

//:---------------------------------
STAFCV_T topJoin:: reset() {
   FREE(myWhereClause);
   myWhereClause = NULL;
   topProject::reset();
   EML_SUCCESS(STAFCV_OK);
}


//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: topFactory

//:----------------------------------------------- CTORS & DTOR       --
topFactory:: topFactory()
		: socFactory()
		, socObject() {
   EML_MESSAGE("topFactory -- NULL Creator\n");
}

//:---------------------------------
topFactory:: topFactory(const char * name)
		: socFactory()
		, socObject(name, "topFactory") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//:---------------------------------
topFactory:: ~topFactory() {
}

//:----------------------------------------------- ATTRIBUTES         --
/* **NONE** */

//:----------------------------------------------- INTERFACE METHODS  --
char * topFactory:: list () {
   char tit[] =
                "\n"
                "+-------------------------------------------"
                "-----------------------------------\n"
                "|*********************** "
                "TOP - Table Operators listing"
                " ************************\n"
                "%s\n";

   char *c = socFactory::list();

   char *cc = (char*)MALLOC(strlen(c) +1 + strlen(tit));

   sprintf(cc, tit, c);
   FREE(c);
   return cc;
}

//:- Project -----------------------
STAFCV_T topFactory:: deleteProject (const char * name) {
   if( !soc->deleteObject(name, "topProject") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: findProject (const char * name
		, topProject*& project) {
   socObject* obj=NULL;
   if( NULL == (obj = soc->findObject(name,"topProject")) ){
      project = NULL;   //- ???-leave as is?
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   project = TOPPROJECT(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: getProject (IDREF_T id, topProject*& project) {
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      project = NULL;
      EML_ERROR(INVALID_IDREF);
   }
   if( 0 != strcmp(obj->type(),"topProject") ){
      project = NULL;
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   project = TOPPROJECT(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: newProject (const char * name
		, const char * spec) {
   IDREF_T id;
   if( soc->idObject(name,"topProject",id) ){
      EML_CONTEXT("ERROR: You already have a '%s'.\n",name);
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   if( ! isValidSelectSpec(spec) ) {
      EML_CONTEXT("ERROR: Syntax error in '%s'.\n",spec);
      EML_ERROR(INVALID_SELECT_SPEC);
   }
   static topProject* p;
   p = new topProject(name,spec);
   if( !soc->idObject(name,"topProject",id) ){
      EML_CONTEXT("ERROR: This is not your fault, call Staf programmer.\n");
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}

//:- Join --------------------------
STAFCV_T topFactory:: deleteJoin (const char * name) {
   if( !soc->deleteObject(name, "topJoin") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: findJoin (const char * name
		, topJoin*& join) {
   socObject* obj=NULL;
   if( NULL == (obj = soc->findObject(name,"topJoin")) ){
      join = NULL;   //- ???-leave as is?
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   join = TOPJOIN(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: getJoin (IDREF_T id, topJoin*& join) {
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      join = NULL;
      EML_ERROR(INVALID_IDREF);
   }
   if( 0 != strcmp(obj->type(),"topJoin") ){
      join = NULL;
      EML_CONTEXT("ERROR: We getting mixed up with our object names.\n");
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   join = TOPJOIN(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: findSort (const char * name
		, topSort*& sort) {
  socObject* obj=NULL;
  
  if( NULL == (obj = soc->findObject(name,"topSort")) ){
    sort = NULL;   //- ???-leave as is?
    EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
    EML_ERROR(OBJECT_NOT_FOUND);
  }
  sort = TOPSORT(obj);
  EML_SUCCESS(STAFCV_OK);
}
//:---------------------------------
STAFCV_T topFactory:: newSort (const char * name, const char * whichCol) {
   IDREF_T id;
   if( soc->idObject(name,"topSort",id) ){
      EML_CONTEXT("ERROR: You already have a '%s'.\n",name);
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   topSort* p;
   p = new topSort(name,whichCol);
   if( !soc->idObject(name,"topSort",id) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}
STAFCV_T topFactory:: newJoin (const char * name, const char * spec
		, const char * clause) {
   IDREF_T id;
   if( soc->idObject(name,"topJoin",id) ){
      EML_CONTEXT("ERROR: You already have a '%s'.\n",name);
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   if( ! isValidWhereClause(clause) ) {
      EML_CONTEXT("ERROR: Invalid where-clause '%s'.\n",spec);
      EML_ERROR(INVALID_WHERE_CLAUSE);
  }
   if( ! isValidSelectSpec(spec) ) {
      EML_CONTEXT("ERROR: Invalid selection spec '%s'.\n",spec);
      EML_ERROR(INVALID_SELECT_SPEC);
   }
   static topJoin* p;
   p = new topJoin(name,spec,clause);
   if( !soc->idObject(name,"topJoin",id) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}

//:- Cut ---------------------------
STAFCV_T topFactory:: deleteCut (const char * name) {
   if( !soc->deleteObject(name, "topCut") ){
      EML_ERROR(CANT_DELETE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: findCut (const char * name
		, topCut*& cut) {
   socObject* obj=NULL;
   if( NULL == (obj = soc->findObject(name,"topCut")) ){
      cut = NULL;   //- ???-leave as is?
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   cut = TOPCUT(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: getCut (IDREF_T id, topCut*& cut) {
   socObject* obj;
   if( NULL == (obj = soc->getObject(id)) ){
      cut = NULL;
      EML_ERROR(INVALID_IDREF);
   }
   if( 0 != strcmp(obj->type(),"topCut") ){
      cut = NULL;
      EML_CONTEXT("ERROR: '%s' is of the wrong type.\n",obj->name());
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   cut = TOPCUT(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: newCut (const char * name, const char * spec) {
   IDREF_T id;
   if( soc->idObject(name,"topCut",id) ){
      EML_CONTEXT("ERROR: You already have a '%s'.\n",name);
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static topCut* p;
   p = new topCut(name,spec);
   if( !soc->idObject(name,"topCut",id) ){
      EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

