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
#include "top_utils.h"

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
      mySelectSpec = (char*)ASUALLOC(strlen(spec) +1);
      strcpy(mySelectSpec, spec);
   }
   else {
      mySelectSpec = NULL;
   }
}

//:---------------------------------
topProject:: ~topProject(){
   ASUFREE(mySelectSpec);
};

//:----------------------------------------------- ATTRIBUTES         --
char* topProject:: selectionSpecification() {
   char* c=NULL;
   c = (char*)ASUALLOC(strlen(mySelectSpec) +1);
   strcpy(c,mySelectSpec);
   return c;
}

//----------------------------------
void topProject:: selectionSpecification(const char* spec) {
   if( isValidSelectSpec((char*)spec) ){
      ASUFREE(mySelectSpec);
      mySelectSpec = (char*)ASUALLOC(strlen(spec) +1);
      strcpy(mySelectSpec,spec);
   }
}

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T topProject:: project(tdmTable * table1, tdmTable *& table2) {
   DS_DATASET_T *pTbl1=table1->dslPointer();
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
      free(n);
      EML_ERROR(CANT_CREATE_TABLE);
   }
   free(n);
// table2 = new tdmTable(pTbl2);		// HACK !!!
   char *n2=NULL;
   char *s2=NULL;
   if( !dsTableName(&n2,pTbl2) 
   ||  !dsTableTypeSpecifier(&s2,pTbl2) 
   ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
   tdm->newTable(n2,s2,0);
   if( !tdm->findTable(n2, table2) ){
      EML_ERROR(CANT_FIND_OBJECT);
   }
   return table2;
}

//----------------------------------
STAFCV_T topProject:: reset() {
   ASUFREE(mySelectSpec);
   mySelectSpec = NULL;
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
      mySelectSpec = (char*)ASUALLOC(strlen(spec) +1);
      strcpy(mySelectSpec, spec);
   }
   else {
      mySelectSpec = NULL;
   }
   if( isValidWhereClause((char*)clause) ){
      myWhereClause = (char*)ASUALLOC(strlen(clause) +1);
      strcpy(myWhereClause, clause);
   }
   else {
      myWhereClause = NULL;
   }
}

//:---------------------------------
topJoin:: ~topJoin(){
   ASUFREE(myWhereClause);
};

//:----------------------------------------------- ATTRIBUTES         --
char* topJoin:: whereClause() {
   char* c=NULL;
   c = (char*)ASUALLOC(strlen(myWhereClause) +1);
   strcpy(c,myWhereClause);
   return c;
}

//:---------------------------------
void topJoin:: whereClause(const char* clause) {
   if( isValidWhereClause((char*)clause) ){
      ASUFREE(myWhereClause);
      myWhereClause = (char*)ASUALLOC(strlen(clause) +1);
      strcpy(myWhereClause,clause);
   }
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
      free(n);
      EML_ERROR(CANT_CREATE_TABLE);
   }
   free(n);
// table3 = new tdmTable(pTbl3);                // HACK !!!
   char *n3=NULL;
   char *s3=NULL;
   if( !dsTableName(&n3,pTbl3) 
   ||  !dsTableTypeSpecifier(&s3,pTbl3) 
   ){
      EML_ERROR(CANT_CREATE_TABLE);
   }
   tdm->newTable(n3,s3,0);
   if( !tdm->findTable(n3, table3) ){
      EML_ERROR(CANT_FIND_OBJECT);
   }
   return table3;
}

//:---------------------------------
STAFCV_T topJoin:: reset() {
   ASUFREE(myWhereClause);
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
   EML_MESSAGE(topFactory -- NULL Creator);
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
   return NULL; //HACK
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
   if( !soc->findObject(name,"topProject",obj) ){
      project = NULL;   //- ???-leave as is?
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   project = TOPPROJECT(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: getProject (IDREF_T id, topProject*& project) {
   socObject* obj;
   if( !soc->getObject(id,obj) ){
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
   if( !soc->findObject(name,"topJoin",obj) ){
      join = NULL;   //- ???-leave as is?
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   join = TOPJOIN(obj);
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T topFactory:: getJoin (IDREF_T id, topJoin*& join) {
   socObject* obj;
   if( !soc->getObject(id,obj) ){
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

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

