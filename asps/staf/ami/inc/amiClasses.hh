/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        amiClasses.hh
**:DESCRIPTION: AMI Classes
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     12dec95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#ifdef __cplusplus
#ifndef AMICLASSES_HH
#define AMICLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include "asuLib.h"
#include "socLib.h"
#include "ami_macros.h"
#include "ami_types.h"

//:----------------------------------------------- TYPEDEFS           --

//:=============================================== CLASS              ==
class amiInvoker: public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   amiInvoker(const char * name, long rank
		, FNC_PTR_T pam
		, const STRING_SEQ_T& specs);
   virtual ~amiInvoker();

//:----------------------------------------------- ATTRIBUTES         --
   virtual long rank ();
   virtual FNC_PTR_T pFunction ();

//- OVERRIDE VIRTUAL
   virtual char * listing();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);

   virtual STAFCV_T call (TABLE_SEQ_T& tbl);

   virtual STAFCV_T init ();
   virtual STAFCV_T start ();
   virtual STAFCV_T stop ();

   virtual char * tableSpec (long ntbl);
   virtual AMI_IO_MODE_T tableMode (long ntbl);

protected:
//:----------------------------------------------- PROT VARIABLES     --
   long myRank;		// storage of PAM function rank (# of tables)
   char ** myTblSpecs;	// storage of table spec.s
   FNC_PTR_T myPamFtn;	// storage of PAM function pointer

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

};

//:=============================================== CLASS              ==
class amiBroker: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   amiBroker(const char * name);
   virtual ~amiBroker();

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);
   virtual char * list ();

   virtual STAFCV_T callInvoker (const char * name
		, const STRING_SEQ_T& tables);

   virtual STAFCV_T deleteInvoker (const char * name);
   virtual amiInvoker* findInvoker (const char * name);
   virtual amiInvoker* getInvoker (IDREF_T id);
   virtual amiInvoker* newInvoker (const char * name
		, long rank
		, FNC_PTR_T pam
		, const STRING_SEQ_T& specs);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
//:----------------------------------------------- PRIV FUNCTIONS     --

};

//----------------------------------------------------------------------
CORBA_TIE(amiInvoker)
CORBA_TIE(amiBroker)

#endif /*AMICLASSES_HH*/
#endif /*__cplusplus*/

