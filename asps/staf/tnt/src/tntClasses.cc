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
#include "hbkCWN.h"

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
   tntNtuple:: tntNtuple(long id)
		: socObject(id, "tntNtuple") {
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
   char *t;
   t = hbkCWNtitle(hid());
   int i=strlen(t)-1;
   while( ' ' == t[i] ){
      t[i--] = 0;
   }
   return t;
}

//:---------------------------------
long tntNtuple::  entryCount () {
   return hbkCWNentryCount(hid());
}

//:---------------------------------
long tntNtuple::  columnCount () {
   return hbkCWNcolumnCount(hid());
}

//:----------------------------------------------- INTERFACE METHODS  --
//:---------------------------------
STAFCV_T tntNtuple:: fill (tdmTable* table) {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
STAFCV_T tntNtuple:: append (tdmTable* table) {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
STAFCV_T tntNtuple:: clear () {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
STAFCV_T tntNtuple:: export (tdmTable* table) {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
char * tntNtuple:: tag (long iColumn) {
   char *t;
   if( iColumn < 0 || columnCount() <= iColumn ){
      EML_ERROR(INVALID COLUMN);
   }
   t = hbkCWNcolumnTag(hid(),iColumn);
   return t;
}

//:---------------------------------
STAFCV_T tntNtuple:: getDataFromTable (tdmTable* table) {
   EML_ERROR(PURE_VIRTUAL);
}

//:---------------------------------
STAFCV_T tntNtuple:: putDataToTable (tdmTable* table) {
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
//: tntCWNtuple

//:----------------------------------------------- CTORS & DTOR       --
   tntCWNtuple:: tntCWNtuple()
                : socObject("NULL","tntCWNtuple") {
      myPtr = (SOC_PTR_T)this;
   }

//:---------------------------------
   tntCWNtuple:: tntCWNtuple(long id, tdmTable* table)
                : socObject(id, "tntCWNtuple") {
      myPtr = (SOC_PTR_T)this;

//-WARNING-This works only in a collocated process.
      if( !dsl2cwn(table->dslPointer(),id) ){
	 myHid = id;
//-BUG?	 EML_MESSAGE(CANT_CREATE_CWN);
      }
      myHid = id;
      dslSpec = (char*)(table->typeSpecifier());
/*-- NEW CONSTRUCTOR --

-- END NEW CONSTRUCTOR --*/
   }

//:---------------------------------
   tntCWNtuple:: tntCWNtuple(long id)
		: socObject(id, "tntCWNtuple") {
      myPtr = (SOC_PTR_T)this;

   char s[128];					// max chform size
      myHid = id;
/*-- HACK: now identify HBOOK ID and get characteristics
      numBlocks = hbkCWNblockCount(hid());
      chforms = new char*[numBlocks];
      blockNames = new char*[numBlocks];
      for( int i=0;i<numBlocks;i++ ){
	 blockNames[i] = (char*)malloc(9);	// 8-char names???
	 strncpy(blockNames[i],hbkCWNblockName(hid(),i),8);
	 strncpy(s,hbkCWNblockChform(hid(),i),127);
	 chform[i] = (char*)malloc(strlen(s) +1);
	 strncpy(chform[i],s, strlen(s));
      }
---- HACK: end of HACK */
   }
//:---------------------------------
   tntCWNtuple:: ~tntCWNtuple(){ };

//:----------------------------------------------- ATTRIBUTES         --
long tntCWNtuple::  blockCount () {
   return numBlocks;
}

//:----------------------------------------------- INTERFACE METHODS  --
//:---------------------------------
char * tntCWNtuple:: blockChform (long iBlock) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
char * tntCWNtuple:: blockName (long iBlock) {
   char *c=NULL;
   if( 0 > iBlock || iBlock >= blockCount() ){
      EML_ERROR(INVALID_BLOCK);
   }
   c = (char*)malloc(strlen(blockNames[iBlock]) +1);
   strncpy(c, blockNames[iBlock], strlen(blockNames[iBlock]));
   return c;
}

//:---------------------------------
STAFCV_T tntCWNtuple:: fill (tdmTable* table) {
   if( !clear()
   ||  !append(table)
   ){
      EML_ERROR(HBOOK_ERROR);
   }
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T tntCWNtuple:: append (tdmTable* table) {
   int ic,ir,ib;
   static long offsets[32];	//HACK 32 blocks
   if( !table->isType(dslSpec) ){
       EML_ERROR(WRONG_TABLE_TYPE);	// compatable???
   }
   TDM_DATABLOCK_T data;
   if( !table->getData(data) ){
      EML_ERROR(BAD_TABLE_DATA);
   }
   long stride = table->rowSize();	// in bytes;

   unsigned char *buff;
   for( ir=0;ir<table->rowCount();ir++ ){
      for( ib=0;ib<blockCount();ib++ ){
	 buff = data._buffer + stride*ir + (abs(offsets[ib])-1);
	 hbkCWNputRowBlock(hid(),ir,ib);	/*,(char*)buff);*/
      }
   }
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T tntCWNtuple:: clear () {
   if( !hbkCWNclear(hid()) ){
      EML_ERROR(HBOOK_ERROR);
   }
   EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T tntCWNtuple:: export (tdmTable* table) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
   for( int ir=0;ir<hbkCWNentryCount(hid());ir++ ){
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! WORK!!!
   }
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
long tntCWNtuple:: blockOffset (long iblock) {
   return hbkCWNblockOffset(hid(),iblock);
/* hbkCWNblockSize(hid(),iblock); */

/* ALTERNATIVE APPROACH
   static unsigned char filled=FALSE;
   static long offsets[32];	//HACK 32 blocks
   if( !filled ){
      filled = TRUE;
      offsets[0] = 0;
      tdmTable *table = new tdmTable("tmp233",dslSpec,0);
      int nblock = 1;
      for( ic=1;ic<table->columnCount();ic++ ){
	 if( (DS_TYPE_CHAR == table->columnTypeCode(ic) 
	 &&   DS_TYPE_CHAR != table->columnTypeCode(ic-1))
	 ||  (DS_TYPE_CHAR != table->columnTypeCode(ic) 
	 &&   DS_TYPE_CHAR == table->columnTypeCode(ic-1))
	 ){
	    nblock++;
	    offsets[nblock] = offsets[nblock -1];
	 }
	 offsets[nblock] += table->columnSize(ic);
      }
      delete table;
   }
   return offsets[iblock];
ENDOF ALTERNATIVE APPROACH */
}

//:---------------------------------
unsigned char tntCWNtuple:: isCharBlock (long iblock) {

   if( hbkCWNblockIsChar(hid(),iblock) ){
      return TRUE;
   }
   return FALSE;

/*- ALTERNATIVE APPROACH
   static unsigned char filled=FALSE;
   static unsigned char is_CharBlock[32];	//HACK 32 blocks
   if( !filled ){
      filled = TRUE;
      is_CharBlock[0] = (DS_TYPE_CHAR == table->columnTypeCode(0));
      tdmTable *table = new tdmTable("tmp285",dslSpec,0);
      int nblock = 1;
      for( int ic=1;ic<table->columnCount();ic++ ){
//- New CHAR block
	 if( DS_TYPE_CHAR == table->columnTypeCode(ic) 
	 &&  !is_CharBlock[nblock-1]
	 ){
	    nblock++;
	    is_CharBlock[nblock] = TRUE;
	 }
//- New NUMB block
	 if( DS_TYPE_CHAR != table->columnTypeCode(ic) 
	 &&  is_CharBlock[nblock-1]
	 ){
	    nblock++;
	    is_CharBlock[nblock] = FALSE;
	 }
      }
      delete table;
   }
   return is_CharBlock[iblock];
ENDOF ALTERNATIVE APPROACH -*/
}

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
   EML_MESSAGE("tntFactory -- NULL Creator\n");
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

//:----------------------------------------------- INTERFACE METHODS  --
char * tntFactory:: list () {
   return NULL; //NOT_YET_IMPLEMENTED
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

