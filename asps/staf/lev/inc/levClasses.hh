//:>--------------------------------------------------------------------
//:FILE:        levClasses.hh
//:DESCRIPTION: LEV Classes
//:AUTHOR:      H. Ward, ward@physics.utexas.edu
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     01jul96, creation
//:<--------------------------------------------------------------------

#ifndef LEVCLASSES_HH
#define LEVCLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include "tdmLib.h"

//:----------------------------------------------- TYPEDEFS           --

//:=============================================== CLASS              ==
class levFactory: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   levFactory(const char * name);
   virtual ~levFactory();

//:----------------------------------------------- ATTRIBUTES         --
   virtual tdmTable * environment();
   virtual tdmTable * versions();

//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);
   virtual char *version();

   virtual void levUpdate();
   virtual STAFCV_T levRegisterEnvironment();
   virtual STAFCV_T update();
   virtual STAFCV_T registerVersion(const char *name, const char *type,
      const char *version);

protected:
//:----------------------------------------------- PROT VARIABLES     --
tdmTable *myEnvironment;
tdmTable *myVersions;

//:----------------------------------------------- PROT FUNCTIONS     --
// **NONE**

private:
//:----------------------------------------------- PRIV VARIABLES     --
// **NONE**
//:----------------------------------------------- PRIV FUNCTIONS     --
// **NONE**

};

//:=============================================== CLASS              ==
CORBA_TIE(levFactory)

#endif /*LEVCLASSES_HH*/

