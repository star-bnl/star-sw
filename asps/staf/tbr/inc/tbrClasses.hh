//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        tbrClasses.hh
//:DESCRIPTION: TBR Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     19jun96-v001a-cet- remove ASCII viewer
//:HISTORY:     11mar96-v000a-cet,hjw- creation
//:<--------------------------------------------------------------------

#ifndef TBRCLASSES_HH
#define TBRCLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include "tdmLib.h"

//:----------------------------------------------- TYPEDEFS           --

//:####################################################################
//:=============================================== CLASS              ==
class tbrMotifViewer: public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tbrMotifViewer(const char * name);
   virtual ~tbrMotifViewer();

//:----------------------------------------------- ATTRIBUTES         --

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual STAFCV_T viewDataset (tdmDataset * dataset);
   virtual STAFCV_T viewTable (tdmTable * table);

protected:
//:----------------------------------------------- PRIV VARIABLES     --

//:----------------------------------------------- PRIV FUNCTIONS     --

};

//:####################################################################
//:=============================================== CLASS              ==
class tbrFactory: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tbrFactory(const char * name);
   virtual ~tbrFactory();

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual STAFCV_T deleteMotifViewer (const char * name);
   virtual STAFCV_T findMotifViewer (IDREF_T id
		, tbrMotifViewer*& viewer);
   virtual STAFCV_T getMotifViewer (const char * name
		, tbrMotifViewer*& viewer);
   virtual STAFCV_T newMotifViewer (const char * name);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
//:----------------------------------------------- PRIV FUNCTIONS     --
};

//----------------------------------------------------------------------
CORBA_TIE(tbrMotifViewer)
CORBA_TIE(tbrFactory)

#endif /*TBRCLASSES_HH*/

