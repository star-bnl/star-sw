/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        socClasses.hh
**:DESCRIPTION: Service and Object Catalog
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     13nov95-v001a-cet- update
**:HISTORY:     21jul95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#ifdef __cplusplus
#ifndef SOCCLASSES_HH
#define SOCCLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include <stream.h>
#include "stafCorba.h"
#include "asu_types.h"
#include "asu_globals.h"
#include "sutClasses.hh"
#include "soc_macros.h"
#include "soc_types.h"

//:#####################################################################
//:=============================================== CLASS              ==
class socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   socObject();
   socObject(long id);
   socObject(const char* name);
   socObject(const char* name, const char* type);
   socObject(long n, const char* type); //automatic name=type"n"
   virtual ~socObject();
//:----------------------------------------------- ATTRIBUTES         --
   virtual IDREF_T idRef ();
   virtual char * name ();
   virtual char * type ();
   virtual SOC_PTR_T ptr ();
   virtual char * soRef ();
   virtual void lock (unsigned char lock);
   virtual unsigned char lock ();
//:----------------------------------------------- PUB FUNCTIONS      --
   virtual STAFCV_T attach ();
   virtual STAFCV_T release ();

//:----------------------------------------------- PRIV VARIABLES     --
protected:
SOC_PTR_T myPtr;
private:
IDREF_T myIdRef;
string *myName;
string *myType;
string *mySOR;
unsigned char myLock;

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:=============================================== CLASS              ==
class socFactory: public virtual socObject { 

public:
//:----------------------------------------------- CTORS & DTOR       --
   socFactory(long maxCount=OBJ_MAX_COUNT);
   socFactory(const char * name, const char * type);
   virtual ~socFactory();

//:----------------------------------------------- ATTRIBUTES         --
   virtual long count ();
   virtual long maxCount ();

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual char * list ();

   virtual STAFCV_T addEntry (IDREF_T idRef);
   virtual STAFCV_T deleteEntry (IDREF_T idRef);
   virtual IDREF_T entry(long n);

protected:
//:----------------------------------------------- PROT VARIABLES     --
   long myCount;
   long myMaxCount;

   IDREF_T* idRefs;

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**
private:
//:----------------------------------------------- PRIV VARIABLES     --
//:**NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:=============================================== CLASS              ==
class socCatalog:  public virtual socFactory {

public:

//:----------------------------------------------- CTORS & DTOR       --
   socCatalog();
// socCatalog(const char* name, const char* type);
   virtual ~socCatalog();

//:----------------------------------------------- ATTRIBUTES         --

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual char * list (); // override virtual
   virtual STAFCV_T deleteID (IDREF_T id);

   virtual STAFCV_T deleteObject (const char * name, const char * type);

   virtual STAFCV_T findObject (const char * name, const char * type
		, socObject*& obj);

   virtual STAFCV_T getObject (IDREF_T id, socObject*& obj);

   virtual STAFCV_T idObject (const char * name
		, const char * type, IDREF_T& id);

   virtual STAFCV_T newObject (const char * name);

   virtual STAFCV_T signIn (socObject* obj, IDREF_T& id);

   virtual STAFCV_T signOut (IDREF_T id);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
socObject**  myObjs;

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

};

//:---------------------------------------------------------------------
CORBA_TIE(socObject)
CORBA_TIE(socCatalog)
CORBA_TIE(socFactory)

#endif /* SOCCLASSES_HH */
#endif /*__cplusplus*/

