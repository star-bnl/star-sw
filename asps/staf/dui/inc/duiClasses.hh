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

//:#####################################################################
//:=============================================== CLASS              ==
class duiFactory: public virtual tdmFactory {

public:
//:----------------------------------------------- CTORS & DTOR       --
   duiFactory(const char * name);
   virtual ~duiFactory();

//:----------------------------------------------- ATTRIBUTES         --
   virtual void cwd (const char * cwd);
   virtual char * cwd ();
   tdmDataset* cwdDO ();
   tdmDataset* rootDO ();
//:----------------------------------------------- PUB FUNCTIONS      --
//- OVERRIDE VIRTUALS
   virtual unsigned char implementsInterface (const char * iface);

//----------------------------------
// Unix-Like commands
   virtual STAFCV_T du (const char * dirPath,long minsize);
   virtual STAFCV_T precious ();
   virtual STAFCV_T rm_nonprecious ();
   virtual STAFCV_T df (char *markerString);
   virtual STAFCV_T cd (const char * dirPath);
   virtual STAFCV_T ln (const char * fromPath, const char * toPath); 
   virtual STAFCV_T cp (const char * fromPath, const char * toPath);
   virtual STAFCV_T append (const char * fromPath, const char * toPath);
   virtual char * ls (const char * path);
   virtual STAFCV_T mkdir (const char * dirPath);
   virtual STAFCV_T mv (const char * fromPath, const char * toPath);
   virtual char * pwd ();
   virtual STAFCV_T rm ( const char * filePath);
   virtual STAFCV_T rmdir (const char * dirPath);

//----------------------------------
// Unix-Related commands
   virtual char * cvtRelAbs (const char * relPath);
   virtual STAFCV_T unlinkAndMaybeFreeMemory (char freeTheMemory,
       const char * filePath);
//- cet-14nov97 make ln look like ln
   virtual STAFCV_T lnmv (char unLinkSrc, const char * fromPath, 
          const char * toPath);  // The unLinkSrc arg allows mv() to be
                                 // simply lnmv(TRUE,xx,xx).
				 // and ln to be lnmv(FALSE,xx,xx).
//-cet-17nov97-from HJWs df,du version
   virtual void duiSprinfWithCommas(char *out,long in);
   virtual STAFCV_T duRecurse (char *path,int indent,
		DS_DATASET_T *pDS, long minsize,int control);


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
   long totBytes;  // used with the du command
   char *preciousList;  // list of precious tables, newline-delimited
   char *current_list;  // list of current  tables, newline-delimited


//:----------------------------------------------- PRIV FUNCTIONS     --
   virtual STAFCV_T findNode_ds (const char * path
		, DS_DATASET_T*& pNode);

};

//----------------------------------------------------------------------
CORBA_TIE(duiFactory)

#endif /*DUICLASSES_HH*/
#endif /*__cplusplus*/

