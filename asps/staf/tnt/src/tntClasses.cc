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
#include <iostream.h>
#include "asuAlloc.h"
#include "tntClasses.hh"
#include "tntHBOOK.h"
#include "hbkCWN.h"

//:----------------------------------------------- MACROS             --
//:----------------------------------------------- PROTOTYPES         --
char *tntColumnChform(tdmTable *, long); 
extern "C" 
{
  const char *id2name(const char *base, long id);
  CWN_BLOCK_TYPE_T block_type(DS_TYPE_CODE_T);
}

//:#####################################################################
//:=============================================== CLASS              ==
//: tntNtuple

//:----------------------------------------------- CTORS & DTOR       --
tntNtuple::tntNtuple()
  : socObject("NULL","tntNtuple") {
  myPtr = 0;
}

//:---------------------------------
tntNtuple::tntNtuple(long id)
  : socObject(id, "tntNtuple") {
  myPtr = (SOC_PTR_T)this;
}

//:---------------------------------
tntNtuple::~tntNtuple(){ };

//:----------------------------------------------- ATTRIBUTES         --
long 
tntNtuple::hid () {
  return myHid;
}

//:---------------------------------
char * 
tntNtuple::title () {
  char *t;

  t = hbkCWNtitle(hid());
  //  int i = strlen(t) - 1; // Strip off any trailing whitespace
  int i=strlen(t);
  if(i > 0) i--;    /*fix read bad index -akio*/
  while( ' ' == t[i] ){
    t[i--] = 0;
  }
  return t;
}

//:---------------------------------
long 
tntNtuple::entryCount () {
  return hbkCWNentryCount(hid());
}

//:---------------------------------
long 
tntNtuple::columnCount () {
  return hbkCWNcolumnCount(hid());
}

//:----------------------------------------------- INTERFACE METHODS  --
//:---------------------------------
char * 
tntNtuple::tag (long iColumn) {
  char *t;
  
  if (iColumn < 0 || columnCount() <= iColumn) {
    EML_ERROR(INVALID COLUMN);
  }
  t = hbkCWNcolumnTag(hid(),(size_t)iColumn);
  
  return t;
}

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

//:#####################################################################
//:=============================================== CLASS              ==
//: tntCWNtuple

//:----------------------------------------------- CTORS & DTOR       --
tntCWNtuple::tntCWNtuple()
  : socObject("NULL","tntCWNtuple") {
  myPtr = (SOC_PTR_T)this;
}

//:---------------------------------
tntCWNtuple::tntCWNtuple(long id, tdmTable *table)
  : socObject(id, "tntCWNtuple") {
  
  CWN_BLOCK_TYPE_T oldBlockType, currentBlockType;
  char *cp;
  long i;
  long iBlock;
  size_t newlen;
  size_t longwordifiedSize, offset;

  // Store this table's dslSpec for reference
  dslSpec = table->typeSpecifier();
  
  // Determine the number of blocks needed for the CWN.
  numBlocks = 0;
  oldBlockType = UNDEFINED;
  for (i = 0; i < table->columnCount(); i++) {
    currentBlockType = block_type(table->columnTypeCode(i));
    if (currentBlockType != oldBlockType) {
      numBlocks++;
      oldBlockType = currentBlockType;
    }
  }

  // Find out longwordified size of a row and MALLOC a buffer
  longwordifiedSize = tntLongwordifyRowSize(table);
  rowBuffer = (char *) MALLOC(longwordifiedSize);

  // Calculate and store block quantities
  _blockPtr = (char **) CALLOC((size_t) numBlocks, sizeof(char*));
  chforms = (char **) CALLOC((size_t) numBlocks, sizeof(char *));
  _blockName = (char **) CALLOC((size_t) numBlocks, sizeof(char *));
  _blockType = (CWN_BLOCK_TYPE_T *) CALLOC((size_t) numBlocks,
					   sizeof(CWN_BLOCK_TYPE_T));
  for (i = 0; i < numBlocks; i++) {
    _blockName[i] = (char *)MALLOC(9);
  }

  offset = 0;
  iBlock = -1;
  oldBlockType = UNDEFINED;
  for (i = 0; i < table->columnCount(); i++) {
    currentBlockType = block_type(table->columnTypeCode(i));
    if (currentBlockType != oldBlockType) {
      iBlock++;
      _blockPtr[iBlock] = rowBuffer + offset;
      _blockType[iBlock] = currentBlockType;
      sprintf(_blockName[iBlock],"BLOCK%3ld",iBlock);
      oldBlockType = currentBlockType;
      chforms[iBlock] = tntColumnChform(table, i);
    } else {
      cp = tntColumnChform(table, i);
      newlen = strlen(chforms[iBlock]) + strlen(cp) + 2;
      chforms[iBlock] = (char *) REALLOC(chforms[iBlock], newlen); 
      strcat(chforms[iBlock], ",");
      strcat(chforms[iBlock], cp);
      FREE(cp);
    }
    offset += tntLongwordifyColumnSize(table, i);
  }
  myPtr = (SOC_PTR_T)this;
  myHid = id;

  // OK, book the CWN and describe the blocks
  char *c; /*fix memory leak -akio*/
  hbkCWNbook(hid(), c=table->dslName());
  FREE(c); /*fix memory leak -akio*/
  for (i = 0; i < numBlocks; i++) {
    if (isCharBlock(i)) {
      hbkCWNcharBlock(hid(),_blockName[i],_blockPtr[i],chforms[i]);
    } else {
      hbkCWNscalarBlock(hid(),_blockName[i],(int *)_blockPtr[i],chforms[i]);
    }
  }

  // Now that the CWN is booked, put some data in it
  import(table);
}

//:---------------------------------
tntCWNtuple::tntCWNtuple(long hid)
  : socObject(hid, "tntCWNtuple") {
  myPtr = (SOC_PTR_T)this;
  
  myHid = hid;
}

//:---------------------------------
tntCWNtuple::~tntCWNtuple() { 
  int i;
  FREE(dslSpec);
  FREE(_blockPtr);
  FREE(_blockType);
  for (i = 0; i < numBlocks; i++) {
    FREE(_blockName[i]); /*fix memory leak -akio*/
    FREE(chforms[i]);
  }
  FREE(_blockName); /*fix memory leak -akio/phenix*/
};

//:----------------------------------------------- ATTRIBUTES         --
long 
tntCWNtuple::blockCount () {
   return numBlocks;
}

//:----------------------------------------------- INTERFACE METHODS  --
//:---------------------------------
char * 
tntCWNtuple::blockChform (long iBlock) {
  char *c = 0;

  if (iBlock < 0 || iBlock >= blockCount()) {
    EML_ERROR(INVALID_BLOCK);
  }
  c = strdup(chforms[iBlock]);

  return c;
}

//:---------------------------------
char * 
tntCWNtuple::blockName (long iBlock) {
  char *c = NULL;

  if( iBlock < 0 || iBlock >= blockCount() ){
    EML_ERROR(INVALID_BLOCK);
  }
  c = strdup(_blockName[iBlock]);

  return c;
}

//:---------------------------------
STAFCV_T 
tntCWNtuple::import (tdmTable* table) {
  if (!clear() || !append(table)) {
    EML_ERROR(HBOOK_ERROR);
  }
  EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
// The basic idea here is to loop over the rows and columns of the
// table and copy the values into the buffer area that's been set
// aside for the CWN.  Some difficult parts arise when you have to
// deal with arrays in a column.  For most types, this just introduces
// a new level of nested looping with the conversion of each instance
// to the appropriate longword equivalent, but we handle arrays of
// char differently.
STAFCV_T 
tntCWNtuple::append (tdmTable* table) {
  int i, j, k;
  register long colcnt;
  long *bufferPtr, *lPtr;
  char *c1Ptr, *c2Ptr;
  short *sPtr;
  unsigned char *oPtr;
  unsigned short *usPtr;
  unsigned long *ulPtr;
  float *fPtr;
  double *dPtr;
  size_t colSize, elementSize;
  TDM_CELLDATA_T data;

  if (!table->isType(dslSpec)) {
    EML_ERROR(WRONG_TABLE_TYPE);	// compatable??? done
  }
  
  colcnt=table->columnCount();
  // Loop over the rows of the table ...
  for (i = 0; i < table->rowCount(); i++) {

    bufferPtr = (long *)rowBuffer;

    // ... and loop over the columns of each row
    for (j = 0; j < colcnt; j++) {

      // Figure out the longword aligned size
      colSize = tntLongwordifyColumnSize(table,j);
      elementSize = colSize / table->columnElcount(j);
      memset((void *)bufferPtr, ' ', colSize);

      if(table->getCell(data,i,j)!=STAFCV_OK) printf("getCell failed.\n");
      switch (table->columnTypeCode(j)) {
      case DS_TYPE_CHAR:
	// For `char'-like variables, make a one-for-one copy from the
	// input buffer into the rowBuffer.  Arrays of char's in STAF
	// are just that - not C-style strings with a terminating \0.
	c2Ptr = (char *)bufferPtr;
	c1Ptr = data.data.c;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *c2Ptr++ = *c1Ptr++;
	}
	bufferPtr += colSize/sizeof(long);
	break;
      case DS_TYPE_OCTET:
	// For all other types, take each element one at a time and
	// copy into the correponding longword-sized buffer.  This may
	// involve a conversion of the type.  Perhaps there's a way to
	// optimize this better.
	oPtr = data.data.o;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *bufferPtr = *oPtr++;
	  bufferPtr++;
	}
	break;
      case DS_TYPE_SHORT:
	sPtr = data.data.s;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *bufferPtr = *sPtr++;
	  bufferPtr++;
	}
	break;
      case DS_TYPE_U_SHORT:
	usPtr = data.data.us;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *bufferPtr = *usPtr++;
	  bufferPtr++;
	}
	break;
      case DS_TYPE_LONG:
	lPtr = data.data.l;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *bufferPtr = *lPtr++;
	  bufferPtr++;
	}
	break;
      case DS_TYPE_U_LONG:
	ulPtr = data.data.ul;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *bufferPtr = *ulPtr++;
	  bufferPtr++;
	}
	break;
      case DS_TYPE_FLOAT:
	// Make sure the real types aren't converted to longs.
	fPtr = data.data.f;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *(float *)bufferPtr = *fPtr++;
	  bufferPtr += sizeof(float)/sizeof(long);
	}
	break;
      case DS_TYPE_DOUBLE:
	dPtr = data.data.d;
	for (k = 0; k < table->columnElcount(j); k++) {
	  *(double *)bufferPtr = *dPtr++;
	  bufferPtr += sizeof(double)/sizeof(long);
	}
	break;
      case DS_TYPE_STRUCT:
	EML_MESSAGE("Tables containing structs not supported by `tnt'.\n");
	break;
      }
    }

    // Once the rowBuffer has been filled, call HFNT.
    hbkCWNputRow(hid());
  }

  EML_SUCCESS(STAFCV_OK);

}

//:---------------------------------
STAFCV_T 
tntCWNtuple::clear () {
  if (!hbkCWNclear(hid())) {
    EML_ERROR(HBOOK_ERROR);
  }
  EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
STAFCV_T 
tntCWNtuple::export (tdmTable* table) {
  // Just to hush pedantic compilers
  static void *p = table;
  EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:---------------------------------
STAFCV_T 
tntCWNtuple::show () {
  return hbkCWNhprnt(hid());
}

//:---------------------------------
STAFCV_T 
tntCWNtuple::print (long ifirst, long nrows) {
  // Just to hush pedantic compilers
  static void *pi = &ifirst;
  static void *pn = &nrows;
  EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:----------------------------------------------- PROT FUNCTIONS     --
char * 
tntCWNtuple::blockPtr (long iBlock) {
  if (iBlock < 0 || iBlock >= blockCount()) {
    EML_ERROR(INVALID_BLOCK);
  } 

  return _blockPtr[iBlock];
}

//:---------------------------------
unsigned char 
tntCWNtuple::isCharBlock (long iBlock) {
  
  if (iBlock < 0 || iBlock >= numBlocks) {
    EML_ERROR(INVALID_BLOCK);
  } 
  if (_blockType[iBlock] == CHAR_BLOCK) {
    return TRUE;
  }
  
  return FALSE;
}

//:------------------------------
STAFCV_T 
tntCWNtuple::getDataFromTable(tdmTable* table) {
  // Just to hush pedantic compilers
  static void *pt = table;
  EML_ERROR(NOT_YET_IMPLEMENTED);
}

//:------------------------------
STAFCV_T
tntCWNtuple::putDataToTable(tdmTable* table) {
  // Just to hush pedantic compilers
  static void *pt = table;
  EML_ERROR(NOT_YET_IMPLEMENTED);
}


// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**


//:#####################################################################
//:=============================================== CLASS              ==
//: tntFactory

//:----------------------------------------------- CTORS & DTOR       --
tntFactory::tntFactory()
		: socFactory()
		, socObject() {
   EML_MESSAGE("tntFactory -- NULL Creator\n");
}

//:---------------------------------
tntFactory::tntFactory(const char * name)
		: socFactory()
		, socObject(name, "tntFactory") {
   myPtr = (SOC_PTR_T)this;
   lock(TRUE);
}

//:---------------------------------
tntFactory::~tntFactory() {
}

//:----------------------------------------------- ATTRIBUTES         --
// *** NONE ***

//:----------------------------------------------- INTERFACE METHODS  --
char * 
tntFactory::list () {
  char *c = 0, *l = 0, *cc = 0;
  socObject *obj;
  int i, lc;

  c = (char *) MALLOC (80 * (4 + soc->count()));

  // Print the header
  sprintf(c, 
	  "+-------+-----------------+-----------------+----------------------------------\n"
	  "| IDREF | NAME:OBJECT     | TYPE:CLASS      | DESCRIPTION                      \n"
	  "+-------+-----------------+-----------------+----------------------------------\n"
	  );

  // This should become a real iterator
  lc = 3;
  for (i = 0; i < soc->count(); i++) {
    char *s=NULL;  /*fix memory leak -akio*/
    obj = soc->getObject(soc->entryID(i));
    if ((obj != NULL) &&
	(strcmp(s=obj->type(),"tntCWNtuple") == 0)) {
      l = obj->listing();
      cc = c + (80 * lc++);
      sprintf(cc,"%-79s\n",l);
      if(l) FREE(l); /*fix free un-init ptr -akio*/
    }
    if(s) FREE(s); /*fix memory leak -akio*/
  }

  return c;
}

//:---------------------------------
STAFCV_T 
tntFactory::deleteCWNtuple (long hid) {
  const char *name = id2name("tntCWNtuple",hid);
  
  if( !soc->deleteObject(name,"tntCWNtuple") ){
    EML_ERROR(CANT_DELETE_OBJECT);
  }
 
  EML_SUCCESS(STAFCV_OK);
}

//:---------------------------------
tntCWNtuple *
tntFactory::findCWNtuple (long hid) {
  socObject *obj;
  tntCWNtuple *CWNtuple;
  const char *name = id2name("tntCWNtuple",hid);

  obj = soc->findObject(name,"tntCWNtuple");
  if (obj == NULL) {
    CWNtuple = NULL;
    EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",name); 
    EML_ERROR(OBJECT_NOT_FOUND);
  }
  CWNtuple = TNTCWNTUPLE(obj);
  return CWNtuple;
}

//:---------------------------------
tntCWNtuple *
tntFactory::getCWNtuple (IDREF_T id) {
  tntCWNtuple *CWNtuple;
  
  socObject* obj;

  obj = soc->getObject(id);
  if (obj == NULL) {
    CWNtuple = NULL;
    EML_ERROR(OBJECT_NOT_FOUND);
  }
  if (strcmp(obj->type(),"tntCWNtuple") != 0){
    CWNtuple = NULL;
    EML_ERROR(WRONG_OBJECT_TYPE);
  }
  CWNtuple = TNTCWNTUPLE(obj);
  
  return CWNtuple;
}

//:---------------------------------
tntCWNtuple *
tntFactory::newCWNtuple (long hid) {
  IDREF_T id;
  tntCWNtuple *p = NULL;
  const char *name;

  if (findCWNtuple(hid) != NULL) {
    EML_CONTEXT("ERROR: You already have a CWNtuple of HID %d.'.\n",hid);
    EML_ERROR(DUPLICATE_OBJECT);
  }
  p = new tntCWNtuple(hid);
  name = id2name("tntCWNtuple",hid);
  if (!soc->idObject(name, "tntCWNtuple", id)) {
    EML_ERROR(OBJECT_NOT_FOUND);
  }
  addEntry(id);
  
  return p;
}

//:---------------------------------
tntCWNtuple *
tntFactory::createCWNtuple (long hid, tdmTable *table) {
   IDREF_T id;
   const char *name = id2name("tntCWNtuple",hid);

   if( soc->idObject(name,"tntCWNtuple",id) ){
      EML_CONTEXT("ERROR: You already have a ntuple '%d'.\n",hid);
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }

   static tntCWNtuple *p;
   p = new tntCWNtuple(hid,table);
   if( !soc->idObject(name,"tntCWNtuple",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);

   return p;
}

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

// ---------------------------------------------------------------------

