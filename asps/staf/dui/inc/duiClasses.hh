/*:Copyright 1995, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        duiClasses.hh
**:DESCRIPTION: Dataset Unix-like Interface Classes
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     13mar96-v001a-cet- make dependent on tdmFactory
**:HISTORY:     08dec95-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#ifdef __cplusplus
#ifndef DUICLASSES_HH
#define DUICLASSES_HH

//:----------------------------------------------- INCLUDES           --
#include "dstype.h"
#include "asuLib.h"
#include "socLib.h"
#include "tdmLib.h"
#include "dui_macros.h"
#include "dui_types.h"

//:=============================================== CLASS              ==
class duiDispatcher: public virtual tdmFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   duiDispatcher(const char * name);
   virtual ~duiDispatcher();

//:----------------------------------------------- ATTRIBUTES         --
   virtual void cwd (const char * cwd);
   virtual char * cwd ();
   tdmDataset* cwdDO ();
   tdmDataset* rootDO ();
//:----------------------------------------------- PUB FUNCTIONS      --
//----------------------------------
// Unix-Like commands
   virtual STAFCV_T cd (const char * dirPath);
   virtual STAFCV_T cp (const char * fromPath, const char * toPath);
   virtual STAFCV_T ls (const char * path, char *& result);
   virtual STAFCV_T mkdir (const char * dirPath);
   virtual STAFCV_T mv (const char * fromPath, const char * toPath);
   virtual STAFCV_T pwd (char *& result);
   virtual STAFCV_T rm (const char * filePath);
   virtual STAFCV_T rmdir (const char * dirPath);

//----------------------------------
// Unix-Related commands
   virtual char * cvtRelAbs (const char * relPath);

//----------------------------------
// Over-ride tdmFactory methods
   tdmDataset* findDataset (const char * dirPath);
   tdmTable* findTable (const char * filePath);
   tdmDataset* newDataset (const char * name, long setDim);
   tdmTable* newTable (const char * name, const char * spec
   		, long rows);

protected:
//:----------------------------------------------- PRIV VARIABLES     --
   char *myCwd;

   tdmDataset *myRoot;
   DS_DATASET_T *pDSroot;

//:----------------------------------------------- PRIV FUNCTIONS     --
   virtual STAFCV_T findNode_ds (const char * path
		, DS_DATASET_T*& pNode);

};

//----------------------------------------------------------------------
CORBA_TIE(duiDispatcher)

#endif /*DUICLASSES_HH*/
#endif /*__cplusplus*/

