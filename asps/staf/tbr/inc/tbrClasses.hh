//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        tbrClasses.hh
//:DESCRIPTION: TBR Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     11mar96-v000a-cet,hjw- creation
//:<--------------------------------------------------------------------

#ifndef TBRCLASSES_HH
#define TBRCLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include "tdmLib.h"

//:----------------------------------------------- TYPEDEFS           --

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

//:=============================================== CLASS              ==
class tbrAsciiViewer: public virtual socObject {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tbrAsciiViewer(const char * name);
   virtual ~tbrAsciiViewer();

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual STAFCV_T getValue (const char * variable, char *& value);
   virtual STAFCV_T setValue (const char * variable
		, const char * value);
   virtual STAFCV_T printDataset (tdmDataset * dataset);
   virtual STAFCV_T printTable (tdmTable * table);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
//:----------------------------------------------- PRIV FUNCTIONS     --
};

//:=============================================== CLASS              ==
class tbrFactory: public virtual socFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   tbrFactory(const char * name);
   virtual ~tbrFactory();

//:----------------------------------------------- ATTRIBUTES         --
//:**NONE**

//:----------------------------------------------- PUB FUNCTIONS      --
   virtual char * list ();

   virtual STAFCV_T deleteAsciiViewer (const char * name);
   virtual STAFCV_T findAsciiViewer (const char * name
		, tbrAsciiViewer*& viewer);
   virtual STAFCV_T getAsciiViewer (IDREF_T id
		, tbrAsciiViewer*& viewer);
   virtual STAFCV_T newAsciiViewer (const char * name);
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
CORBA_TIE(tbrAsciiViewer)
CORBA_TIE(tbrFactory)

#endif /*TBRCLASSES_HH*/

