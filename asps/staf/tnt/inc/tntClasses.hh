/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:	tntClasses.hh
**:DESCRIPTION:	TNT-Tables to NTuples C++ classes.
**:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:	-- STILL IN DEVELOPMENT --
**:HISTORY:	13jun96-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#ifndef TNTCLASSES_HH
#define TNTCLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include "tdmLib.h"
#include "tnt_macros.h"
#include "tnt_types.h"

typedef enum cwn_block_type_t {
  UNDEFINED=0,
  CHAR_BLOCK,
  NUMB_BLOCK,
  UNKNOWN
}CWN_BLOCK_TYPE_T;

size_t tntLongwordifyColumnSize(tdmTable *, long);
size_t tntLongwordifyRowSize(tdmTable *);

//:#####################################################################
//:=============================================== CLASS              ==
class tntNtuple: public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tntNtuple();
   tntNtuple(long hid);
   virtual ~tntNtuple();

//:----------------------------------------------- ATTRIBUTES         --
   virtual long hid ();
   virtual char * title ();
   virtual long entryCount ();
   virtual long columnCount ();

//:----------------------------------------------- INTERFACE METHODS  --
   virtual STAFCV_T import (tdmTable* table) = 0;
   virtual STAFCV_T append (tdmTable* table) = 0;
   virtual STAFCV_T clear () = 0;
   virtual STAFCV_T export (tdmTable* table) = 0;
   virtual STAFCV_T show () = 0;
   virtual STAFCV_T print (long ifirst, long nrows) = 0;

protected:
//:----------------------------------------------- PROT VARIABLES     --
   long myHid;
   char *rowBuffer;

//:----------------------------------------------- PROT FUNCTIONS     --
   virtual char * tag (long iColumn);
   virtual STAFCV_T getDataFromTable(tdmTable* table) = 0;
   virtual STAFCV_T putDataToTable(tdmTable* table) = 0;

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
//:=============================================== CLASS              ==
class tntCWNtuple: public virtual tntNtuple {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tntCWNtuple();
   tntCWNtuple(long, tdmTable *);
   tntCWNtuple(long hid);
   virtual ~tntCWNtuple();
//:----------------------------------------------- ATTRIBUTES         --
   virtual long blockCount ();

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * blockChform (long iBlock);
   virtual char * blockName (long iBlock);
   
//:- OVERRIDE virtual FUNCTIONS
   virtual STAFCV_T import (tdmTable* table);
   virtual STAFCV_T append (tdmTable* table);
   virtual STAFCV_T clear ();
   virtual STAFCV_T export (tdmTable* table);
   virtual STAFCV_T show ();
   virtual STAFCV_T print (long ifirst, long nrows);

protected:
//:----------------------------------------------- PROT VARIABLES     --
  int numBlocks;
  char **_blockPtr;
  CWN_BLOCK_TYPE_T *_blockType;
  char **_blockName;
  char **chforms;
  char *dslSpec;

//:----------------------------------------------- PROT FUNCTIONS     --
  char *blockPtr(long iblock);
  unsigned char isCharBlock(long iblock);
  STAFCV_T getDataFromTable(tdmTable* table);
  STAFCV_T putDataToTable(tdmTable* table);


private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
//:=============================================== CLASS              ==
class tntFactory: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tntFactory();
   tntFactory(const char * name);
   virtual ~tntFactory();
//:----------------------------------------------- ATTRIBUTES         --
//: **NONE**

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * list();

   virtual STAFCV_T deleteCWNtuple (long hid);
   virtual tntCWNtuple *findCWNtuple (long hid);
   virtual tntCWNtuple *getCWNtuple (IDREF_T id);
   virtual tntCWNtuple *newCWNtuple (long hid);
   virtual tntCWNtuple *createCWNtuple (long hid, tdmTable* table);

protected:
//:----------------------------------------------- PROT VARIABLES     --
//:**NONE**
//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:---------------------------------------------------------------------
CORBA_TIE(tntNtuple)
CORBA_TIE(tntCWNtuple)
CORBA_TIE(tntFactory)

#endif /* TNTCLASSES_HH */

