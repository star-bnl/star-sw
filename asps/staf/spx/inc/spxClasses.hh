/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        spxClasses.hh
**:DESCRIPTION: Implementation classes for SPX.
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     21nov95-v001a-cet- make compatable with socMoast
**:HISTORY:     25oct95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#ifdef __cplusplus
#ifndef SPXCLASSES_HH
#define SPXCLASSES_HH

#include "asuLib.h"
#include "emlLib.h"
#include "socLib.h"
#include "spx_macros.h"
#include "spx_types.h"

//:#####################################################################
//:=============================================== CLASS              ==
class spxDummy: public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   spxDummy(const char* name);
   virtual ~spxDummy();

//:----------------------------------------------- ATTRIBUTES         --
   virtual long nCalls ();

//- override virtual
   virtual char * listing();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);

   virtual STAFCV_T null ();
   virtual STAFCV_T getTime (char *& tim);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
long myNCalls;

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
//:=============================================== CLASS              ==
class spxGrid: public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   spxGrid(const char* name, const short h, const short w);
   virtual ~spxGrid();

//:----------------------------------------------- ATTRIBUTES         --
   virtual short height ();
   virtual short width ();

//- override virtual
   virtual char * listing();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);

   virtual STAFCV_T get (short n, short m, long& value);
   virtual STAFCV_T set (short n, short m, long value);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
short myHeight;
short myWidth;

long **myGrid;

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
//:=============================================== CLASS              ==
class spxFactory: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   spxFactory(const char * name);
   virtual ~spxFactory();

//:----------------------------------------------- ATTRIBUTES         --
//**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);
   virtual char * list ();

   virtual STAFCV_T deleteDummy (const char * name);
   virtual STAFCV_T deleteGrid (const char * name);
   virtual STAFCV_T findDummy (const char * name, spxDummy*& dummy);
   virtual STAFCV_T findGrid (const char * name, spxGrid*& grid);
   virtual STAFCV_T getDummy (IDREF_T id, spxDummy*& dummy);
   virtual STAFCV_T getGrid (IDREF_T id, spxGrid*& grid);
   virtual STAFCV_T newDummy (const char * name);
   virtual STAFCV_T newGrid (const char * name
		, short height, short width);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**

};

//:---------------------------------------------------------------------
CORBA_TIE(spxDummy)
CORBA_TIE(spxGrid)
CORBA_TIE(spxFactory)

#endif /* SPXCLASSES_HH */
#endif /*__cplusplus*/

