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
   virtual char * zebraDir ();

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * tag (long iColumn);
   virtual STAFCV_T getFromTable (tdmTable* table);
   virtual STAFCV_T putToTable (tdmTable* table);
   virtual STAFCV_T show ();
   virtual STAFCV_T print (long ifirst, long nrows);

protected:
//:----------------------------------------------- PROT VARIABLES     --
   long myHid;

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
//:=============================================== CLASS              ==
class tntRWNtuple: public virtual tntNtuple {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tntRWNtuple();
   tntRWNtuple(long hid);
   virtual ~tntRWNtuple();
//:----------------------------------------------- ATTRIBUTES         --
// *** NONE ***
//:----------------------------------------------- INTERFACE METHODS  --
   virtual STAFCV_T getFromTable (tdmTable* table);
   virtual STAFCV_T putToTable (tdmTable* table);
   virtual STAFCV_T show ();
   virtual STAFCV_T print (long ifirst, long nrows);

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

//:#####################################################################
//:=============================================== CLASS              ==
class tntCWNtuple: public virtual tntNtuple {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tntCWNtuple();
   tntCWNtuple(long hid, tdmTable* t);
   virtual ~tntCWNtuple();
//:----------------------------------------------- ATTRIBUTES         --
   virtual long blockCount ();

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * blockName (long iBlock);
   virtual long blockElementCount (long iBlock);
   virtual NT_TYPE_CODE_T columnType (long iColumn);
   
   virtual STAFCV_T getFromTable (tdmTable* table);
   virtual STAFCV_T putToTable (tdmTable* table);
   virtual STAFCV_T show ();
   virtual STAFCV_T print (long ifirst, long nrows);

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

//:#####################################################################
//:=============================================== CLASS              ==
class tntFactory: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tntFactory();
   tntFactory(const char * name);
   virtual ~tntFactory();
//:----------------------------------------------- ATTRIBUTES         --
   virtual char * gName ();

//:----------------------------------------------- INTERFACE METHODS  --
   virtual char * list();
   virtual STAFCV_T paw ();
   virtual STAFCV_T save (const char * filname);
   virtual STAFCV_T share (const char * gname);

   virtual STAFCV_T deleteRWNtuple (long hid);
   virtual STAFCV_T findRWNtuple (long hid, tntRWNtuple*& ntuple);
   virtual STAFCV_T getRWNtuple (IDREF_T id, tntRWNtuple*& ntuple);
   virtual STAFCV_T newRWNtuple (long hid, const char * spec);
   virtual STAFCV_T createRWNtuple (long hid, tdmTable* table);

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
CORBA_TIE(tntRWNtuple)
CORBA_TIE(tntCWNtuple)
CORBA_TIE(tntFactory)

#endif /* TNTCLASSES_HH */

