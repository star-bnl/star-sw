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
   virtual STAFCV_T findDatasetPath (const char * dirPath
		, tdmDataset*& dataset);
   virtual STAFCV_T findTablePath (const char * filePath
		, tdmTable*& table);
   virtual STAFCV_T ls (const char * path, char *& result);
   virtual STAFCV_T mkdir (const char * dirPath);
   virtual STAFCV_T mkTable (const char * filePath
                , const char * spec, long rows);
   virtual STAFCV_T mv (const char * fromPath, const char * toPath);
   virtual STAFCV_T pwd (char *& result);
   virtual STAFCV_T rm (const char * filePath);
   virtual STAFCV_T rmdir (const char * dirPath);

   virtual STAFCV_T findDataset (const char * name
		, tdmDataset*& dataset);
   virtual STAFCV_T findTable (const char * name
		, tdmTable*& table);
//----------------------------------
// Unix-Related commands
   virtual char * cvtRelAbs (const char * relPath);

//----------------------------------

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

