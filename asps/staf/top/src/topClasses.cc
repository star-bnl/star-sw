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
#include "topClasses.hh"
#include "tdmClasses.hh"
#include "top_utils.h"
extern "C" void CutsInit(void);
extern "C" int dsuDoCuts(size_t bytes, char *ba, char *cut
		,DS_DATASET_T *pTab);
extern "C" int dsuRowPassedCuts(char *ba,long row);
extern "C" int IsValidCutFunc(char*);
//:----------------------------------------------- MACROS             --
//:----------------------------------------------- PROTOTYPES         --
extern "C" char* id2name(char* base,long id);

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
   if( isValidSelectSpec((char*)spec) ){
      mySelectSpec = (char*)MALLOC(strlen(spec) +1);
      strcpy(mySelectSpec, spec);
   }
   else {
      mySelectSpec = NULL;
   }
}

//:---------------------------------
topProject:: ~topProject(){
   FREE(mySelectSpec);
};

//:----------------------------------------------- ATTRIBUTES         --
char* topProject:: selectionSpecification() {
   char* c=NULL;
   c = (char*)MALLOC(strlen(mySelectSpec) +1);
   strcpy(c,mySelectSpec);
   return c;
}

//----------------------------------
void topProject:: selectionSpecification(const char* spec) {
   if( isValidSelectSpec((char*)spec) ){
      FREE(mySelectSpec);
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
   char* ss="                           ";
   strncpy(ss,s,strlen(ss)-1);
   cc = (char*)MALLOC(79);
   memset(cc,0,79);
   sprintf(cc,"%s %s",c,ss);
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
   pTbl2=table2->dslPointer();
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
   char *n=NULL;
   if( NULL == name ){
//   n=(char*)id2name("projection",soc->nextIDref());
     n=(char*)id2name("projection",111); // HACK 
   }
   else {
     n=(char*)name;
   }
   if( !dsTargetTable(&pTbl2, n, n, pTbl1, NULL, NULL, 
	     mySelectSpec) ){
      FREE(n);
      EML_ERROR(CANT_CREATE_TABLE);
   }
   FREE(n);
// table2 = new tdmTable(pTbl2);		// HACK !!!
   char *n2=NULL;
   char *s2=NULL;
   if( !dsTableName(&n2,pTbl2) 
   ||  !dsTableTypeSpecifier(&s2,pTbl2) 
   ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
   if( NULL == (table2 = tdm->newTable(n2,s2,0)) ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
   return table2;
}

//----------------------------------
STAFCV_T topProject:: reset() {
   FREE(mySelectSpec);
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
};

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
  if(!dsTableRowSize(&bytesPerRow,dsPtr)) EML_ERROR(CANT_FIND_ROW_SIZE);
  if(!dsTableDataAddress(&beginningOfTable,dsPtr)) EML_ERROR(CANT_FIND_DATA);
  bottomNewTbl=beginningOfTable;

  nbytes=(size_t)(((*orig)/8)+1); mask=MALLOC(nbytes);
  if(!mask) { printf("Could not allocate %d bytes.\n",nbytes); return 0; }

  CutsInit();
  if(!dsuDoCuts(nbytes,(char*)mask,(char*)func,dsPtr)) {
    printf("Failure, check your cuts string for syntax errors:\n");
    printf("%s\n",func);
    return 0;
  }
  startRow=-10;
  for(row=0;row<*orig+1;row++) { /* The <x+1 is deliberate, to invoke the
                                 ** else cluase during ending of the loop. */
    if(row<(*orig)&&dsuRowPassedCuts((char*)mask,row)) {
      if(row%1557==0) printf("Phase II: %d %% complete.\n",(row*100)/(*orig));
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
  printf("%d rows passed the cuts.\n",rowCnt);
  *percentPass=(100.0*rowCnt)/(*orig)+0.5;
  tbl->rowCount(rowCnt);
  return 7;
}
STAFCV_T topCut:: DoFilterTable(tdmTable *src,
    tdmTable *tgt,char *func,long *orig,
    long *percentPass) {
  size_t numBytesToTransfer,bytesPerRow,nbytes; long startRow,row;
  char *beginOfSrcTbl,*bottomNewTbl,*copyThis;
  long numberPass=0,colCnt; void *mask;
  DS_DATASET_T *dsPtr,*tgtPtr;
  colCnt=src->columnCount();
  *orig=src->rowCount();

  dsPtr=src->dslPointer(); // ONLY in a collocated process (CORBA)
  tgtPtr=tgt->dslPointer(); // ONLY in a collocated process (CORBA)
  if(!dsTableRowSize(&bytesPerRow,tgtPtr)) EML_ERROR(CANT_FIND_ROW_SIZE);

  nbytes=(size_t)(((*orig)/8)+1); mask=MALLOC(nbytes);
  if(!mask) { printf("Could not allocate %d bytes.\n",nbytes); return 0; }

  CutsInit();
  if(!dsuDoCuts(nbytes,(char*)mask,(char*)func,dsPtr)) {
    printf("Failure, check your cuts string for syntax errors:\n");
    printf("%s\n",func);
    return 0;
  }
  startRow=-10;
  for(row=0;row<*orig;row++) { /* 1st pass, set rowcount new table*/
    if(dsuRowPassedCuts((char*)mask,row)) (numberPass)++;
  }
  tgt->maxRowCount(numberPass);
  tgt->rowCount((numberPass));
  if(!dsTableDataAddress(&beginOfSrcTbl,dsPtr)) EML_ERROR(CANT_FIND_DATA);
  if(!dsTableDataAddress(&bottomNewTbl,tgtPtr)) EML_ERROR(CANT_FIND_DATA);
  for(row=0;row<(*orig)+1;row++) { /* The <x+1 is deliberate, to invoke the
                                   ** else clause during ending of the loop. */
    if(row<(*orig)&&dsuRowPassedCuts((char*)mask,row)) {
      if(row%1557==0) printf("Phase II: %d %% complete.\n",(row*100)/(*orig));
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
  printf("%d rows passed the cuts.\n",numberPass);
  *percentPass=(100.0*numberPass)/(*orig)+0.5;
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
   printf("Your original table had %d rows.  %d percent of them passed.\n",
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
   printf("Your original table had %d rows.  %d percent of them passed.\n",
   orig,percent);
   EML_SUCCESS(STAFCV_OK);
}


//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

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
   if( isValidSelectSpec((char*)spec) ){
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
};

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
   char* c = topProject::listing();
   char* cc = NULL;
   char* w = whereClause();
   char* ww="                           ";
   strncpy(ww,w,strlen(ww)-1);
   cc = (char*)MALLOC(79);
   memset(cc,0,79);
   sprintf(cc,"%s (%s)",c,ww);
   FREE(c);
   FREE(w);
   return cc;
}

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T topJoin:: join(tdmTable * table1, tdmTable * table2
		, tdmTable *& table3) {
   DS_DATASET_T *pTbl1=table1->dslPointer();
   DS_DATASET_T *pTbl2=table2->dslPointer();
   DS_DATASET_T *pTbl3=NULL;
   if( NULL == table3 ){
      table3 = jTarget(table1, table2, NULL);
   }
   pTbl3=table3->dslPointer();
   if( !dsEquijoin(pTbl3,pTbl1,pTbl2,NULL,myWhereClause,mySelectSpec) ){
      EML_ERROR(JOIN_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);

}

//:---------------------------------
tdmTable * topJoin:: jTarget(tdmTable * table1, tdmTable * table2
		, const char * name) {
   DS_DATASET_T *pTbl1=table1->dslPointer();
   DS_DATASET_T *pTbl2=table2->dslPointer();
   tdmTable * table3=NULL;
   DS_DATASET_T *pTbl3=NULL;
   char *n=NULL;
   if( NULL == name ){
//   n=(char*)id2name("projection",soc->nextIDref());
     n=(char*)id2name("join",111); // HACK 
   }
   else {
     n=(char*)name;
   }
   if( !dsTargetTable(&pTbl3, n, n, pTbl1, pTbl2, NULL, 
             mySelectSpec) ){
      FREE(n);
      EML_ERROR(CANT_CREATE_TABLE);
   }
   FREE(n);
// table3 = new tdmTable(pTbl3);                // HACK !!!
   char *n3=NULL;
   char *s3=NULL;
   if( !dsTableName(&n3,pTbl3) 
   ||  !dsTableTypeSpecifier(&s3,pTbl3) 
   ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
   if( NULL == (table3 = tdm->newTable(n3,s3,0)) ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
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
   char *c = socFactory::list();

   char *cc = (char*)MALLOC(strlen(c) +1 +162);

   sprintf(cc, 
                "\n"
                "+-------------------------------------------"
                "-----------------------------------\n"
                "|*********************** "
                "TOP - Table Operators listing"
                " ************************\n"
                "%s\n",c);
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
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static topProject* p;
   p = new topProject(name,spec);
   if( !soc->idObject(name,"topProject",id) ){
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
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   join = TOPJOIN(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: newJoin (const char * name, const char * spec
		, const char * clause) {
   IDREF_T id;
   if( soc->idObject(name,"topJoin",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static topJoin* p;
   p = new topJoin(name,spec,clause);
   if( !soc->idObject(name,"topJoin",id) ){
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
      EML_ERROR(WRONG_OBJECT_TYPE);
   }
   cut = TOPCUT(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: newCut (const char * name, const char * spec) {
   IDREF_T id;
   if( soc->idObject(name,"topCut",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   static topCut* p;
   p = new topCut(name,spec);
   if( !soc->idObject(name,"topCut",id) ){
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

