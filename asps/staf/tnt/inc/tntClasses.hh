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
   virtual STAFCV_T fill (tdmTable* table);
   virtual STAFCV_T append (tdmTable* table);
   virtual STAFCV_T clear ();
   virtual STAFCV_T export (tdmTable* table);
   virtual STAFCV_T show ();
   virtual STAFCV_T print (long ifirst, long nrows);

protected:
//:----------------------------------------------- PROT VARIABLES     --
   long myHid;
   char *rowBuffer;

//:----------------------------------------------- PROT FUNCTIONS     --
   virtual char * tag (long iColumn);
   virtual STAFCV_T getDataFromTable(tdmTable* table);
   virtual STAFCV_T putDataToTable(tdmTable* table);

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
   tntCWNtuple(long hid, tdmTable* t);
   tntCWNtuple(long hid);
   virtual ~tntCWNtuple();
//:----------------------------------------------- ATTRIBUTES         --
   virtual long blockCount ();

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * blockChform (long iBlock);
   virtual char * blockName (long iBlock);
   
//:- OVERRIDE virutal FUNCTIONS
   virtual STAFCV_T fill (tdmTable* table);
   virtual STAFCV_T append (tdmTable* table);
   virtual STAFCV_T clear ();
   virtual STAFCV_T export (tdmTable* table);
   virtual STAFCV_T show ();
   virtual STAFCV_T print (long ifirst, long nrows);

protected:
//:----------------------------------------------- PROT VARIABLES     --
   int numBlocks;
   char **blockNames;
   char **chforms;
   char *dslSpec;

//:----------------------------------------------- PROT FUNCTIONS     --
   long blockOffset(long iblock);
   unsigned char isCharBlock(long iblock);

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
   virtual STAFCV_T findCWNtuple (long hid, tntCWNtuple*& ntuple);
   virtual STAFCV_T getCWNtuple (IDREF_T id, tntCWNtuple*& ntuple);
   virtual STAFCV_T newCWNtuple (long hid, const char * spec);
   virtual STAFCV_T createCWNtuple (long hid, tdmTable* table);

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

