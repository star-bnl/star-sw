/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tntClasses.C
**:DESCRIPTION: TNT-TEMPLATE ASP C++ code
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     13jun96-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

//:----------------------------------------------- INCLUDES           --
#include "asuAlloc.h"
#include "tntClasses.hh"
#include "tntHBOOK.h"

//:----------------------------------------------- MACROS             --
//:----------------------------------------------- PROTOTYPES         --
extern "C" long dsl2cwn(DS_DATASET_T *pDataset,long hid);
extern "C" char * id2name(char * base, long id);

//:#####################################################################
//:=============================================== CLASS              ==
//: tntNtuple

//:----------------------------------------------- CTORS & DTOR       --
   tntNtuple:: tntNtuple()
		: socObject("NULL","tntNtuple") {
      myPtr = NULL;
   }

//:---------------------------------
   tntNtuple:: tntNtuple(long hid)
		: socObject(hid, "tntNtuple") {
      myPtr = (SOC_PTR_T)this;
   }

//:---------------------------------
   tntNtuple:: ~tntNtuple(){ };

//:----------------------------------------------- ATTRIBUTES         --
long tntNtuple::  hid () {
   return myHid;
}

//:---------------------------------
char * tntNtuple::  title () {
   char tit[80+1];		// HACK-80 
   strncpy(tit,tnt_nt_title_(hid()),80);
   tit[80] = 0;
   int i=79;
   while( ' ' == tit[i] ){
      tit[i] = 0;
   }

   char *result;
   result = (char*)ASUALLOC(strlen(tit) +1);
   strcpy(result,tit);
   return tit;
}

//:---------------------------------
long tntNtuple::  entryCount () {
   return tnt_nt_entry_count_(hid());
}

//:---------------------------------
long tntNtuple::  columnCount () {
   return tnt_nt_column_count_(hid());
}

//:---------------------------------
char * tntNtuple::  zebraDir () {
   return NULL; //NOT_YET_IMPLEMENTED
}

//:----------------------------------------------- INTERFACE METHODS  --
char * tntNtuple:: tag (long iColumn) {
   return NULL; //NOT_YET_IMPLEMENTED
}

//:---------------------------------
STAFCV_T tntNtuple:: getFromTable (tdmTable* table) {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
STAFCV_T tntNtuple:: putToTable (tdmTable* table) {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
STAFCV_T tntNtuple:: show () {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
STAFCV_T tntNtuple:: print (long ifirst, long nrows) {
   EML_ERROR(PURE_VIRTUAL);
}

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: tntRWNtuple

//:----------------------------------------------- CTORS & DTOR       --
   tntRWNtuple:: tntRWNtuple()
                : socObject("NULL","tntRWNtuple") {
      myPtr = (SOC_PTR_T)this;
   }

//:---------------------------------
   tntRWNtuple:: tntRWNtuple(long hid)
                : tntNtuple()
                , socObject(hid, "tntRWNtuple") {
      myPtr = (SOC_PTR_T)this;
   }

//:---------------------------------
   tntRWNtuple:: ~tntRWNtuple(){ };

//:----------------------------------------------- ATTRIBUTES         --
// *** NONE ***

//:----------------------------------------------- INTERFACE METHODS  --
STAFCV_T tntRWNtuple:: getFromTable (tdmTable* table) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntRWNtuple:: putToTable (tdmTable* table) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntRWNtuple:: show () {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntRWNtuple:: print (long ifirst, long nrows) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: tntCWNtuple

//:----------------------------------------------- CTORS & DTOR       --
   tntCWNtuple:: tntCWNtuple()
                : socObject("NULL","tntCWNtuple") {
      myPtr = (SOC_PTR_T)this;
   }

//:---------------------------------
   tntCWNtuple:: tntCWNtuple(long hid, tdmTable* table)
                : socObject(hid, "tntCWNtuple") {
      myPtr = (SOC_PTR_T)this;

//-WARNING-This works only in a collocated process.
      if( !dsl2cwn(table->dslPointer(),hid) ){
	 myHid = hid;
//-BUG?	 EML_MESSAGE(CANT_CREATE_CWN);
      }
      myHid = hid;
   }

//:---------------------------------
   tntCWNtuple:: ~tntCWNtuple(){ };

//:----------------------------------------------- ATTRIBUTES         --
long tntCWNtuple::  blockCount () {
   return -1; /*-TNT_E_UNIMPLEMENTED-*/
}

//:----------------------------------------------- INTERFACE METHODS  --
char * tntCWNtuple:: blockName (long iBlock) {
   return NULL; //NOT_YET_IMPLEMENTED
}

//:---------------------------------
long tntCWNtuple:: blockElementCount (long iBlock) {
   return -1; /*-TNT_E_UNIMPLEMENTED-*/
}

//:---------------------------------
NT_TYPE_CODE_T tntCWNtuple:: columnType (long iColumn) {
   return NT_TYPE_UNKNOWN; //NOT_YET_IMPLEMENTED
}

//:---------------------------------
STAFCV_T tntCWNtuple:: getFromTable (tdmTable* table) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntCWNtuple:: putToTable (tdmTable* table) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntCWNtuple:: show () {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntCWNtuple:: print (long ifirst, long nrows) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**


//:#####################################################################
//:=============================================== CLASS              ==
//: tntFactory

//:----------------------------------------------- CTORS & DTOR       --
tntFactory:: tntFactory()
		: socFactory()
		, socObject() {
   EML_MESSAGE(tntFactory -- NULL Creator);
}

//:---------------------------------
tntFactory:: tntFactory(const char * name)
		: socFactory()
		, socObject(name, "tntFactory") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//:---------------------------------
tntFactory:: ~tntFactory() {
}

//:----------------------------------------------- ATTRIBUTES         --
// *** NONE ***
char * tntFactory::  gName () {
   return NULL; //NOT_YET_IMPLEMENTED
}

//:----------------------------------------------- INTERFACE METHODS  --
char * tntFactory:: list () {
   return NULL; //NOT_YET_IMPLEMENTED
}

//:---------------------------------
STAFCV_T tntFactory:: paw () {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: save (const char * filname) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: share (const char * gname) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: deleteRWNtuple (long hid) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: findRWNtuple (long hid
		, tntRWNtuple*& ntuple) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: getRWNtuple (IDREF_T id, tntRWNtuple*& ntuple) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: newRWNtuple (long hid
		, const char * spec) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: createRWNtuple (long hid
		, tdmTable* table) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: deleteCWNtuple (long hid) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: findCWNtuple (long hid
		, tntCWNtuple*& ntuple) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: getCWNtuple (IDREF_T id, tntCWNtuple*& ntuple) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: newCWNtuple (long hid
		, const char * spec) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T tntFactory:: createCWNtuple (long hid
		, tdmTable* table) {
   IDREF_T id;
   char *name = id2name("tntCWNtuple",hid);
   if( soc->idObject(name,"tntCWNtuple",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }

   static tntCWNtuple *p;
   p = new tntCWNtuple(hid,table);
   if( !soc->idObject(name,"tntCWNtuple",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   free(name);
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}


//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

