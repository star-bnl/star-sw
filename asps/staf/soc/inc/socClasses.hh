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
   virtual char * name (){return myName->copy();}
   virtual const char * Name (){return myName->show();}
   virtual char * type (){return myType->copy();};
   virtual const char * Type (){return myType->show();}
   virtual char * version ();
   virtual const char * Version (){return "dev";}
   virtual SOC_PTR_T ptr ();
   virtual void lock (unsigned char lock){myLock=lock;}
   virtual unsigned char lock (){return myLock;}
   virtual char * listing ();

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual unsigned char implementsInterface (const char * iface);

protected:
//:----------------------------------------------- PROT VARIABLES     --
SOC_PTR_T myPtr;

//:----------------------------------------------- PROT FUNCTIONS     --
//:**NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
IDREF_T myIdRef;
stafString *myName;
stafString *myType;
unsigned char myLock;

//:----------------------------------------------- PRIV FUNCTIONS     --
//:**NONE**
};

//:#####################################################################
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

//- OVER-RIDE VIRTUAL
   virtual char * listing ();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual char * list ();			// list all objects
   virtual unsigned char implementsInterface (const char * iface);

   virtual STAFCV_T addEntry (IDREF_T idRef);
   virtual STAFCV_T deleteEntry (IDREF_T idRef);
   virtual STAFCV_T unaddEntry (IDREF_T idRef);

   virtual IDREF_T entryID(long n);

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

//:#####################################################################
//:=============================================== CLASS              ==
class socCatalog:  public virtual socFactory {

public:

//:----------------------------------------------- CTORS & DTOR       --
   socCatalog();
// socCatalog(const char* name, const char* type);
   virtual ~socCatalog();

//:----------------------------------------------- ATTRIBUTES         --
   virtual char * version (); // override virtual

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual STAFCV_T bind (const char * pname);
   virtual STAFCV_T release (const char * pname);

//- OVERRIDE VIRTUALS
   virtual char * list ();			// list all objects
   virtual unsigned char implementsInterface (const char * iface);

   virtual STAFCV_T deleteID (IDREF_T id);

   virtual STAFCV_T deleteObject (const char * name, const char * type);
   virtual socObject* findObject (const char * name, const char * type);
   virtual socObject* getObject (IDREF_T id);
   virtual STAFCV_T idObject (const char * name, const char * type
		, IDREF_T& id);
   virtual socObject* newObject (const char * name);

   virtual STAFCV_T signIn (socObject* obj, IDREF_T& id);
   virtual STAFCV_T signOut (IDREF_T id);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
socObject**  myObjs;
long nextIDRef;

//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

};

//:---------------------------------------------------------------------
CORBA_TIE(socObject)
CORBA_TIE(socCatalog)
CORBA_TIE(socFactory)

#endif /* SOCCLASSES_HH */
#endif /*__cplusplus*/

