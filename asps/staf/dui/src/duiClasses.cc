//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        duiClasses.C
//:DESCRIPTION: Dataset Unix-like Interface Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     01feb96-v001a-cet- make more like tdmFactory
//:HISTORY:     08dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- INCLUDES           --
#include <string.h>
#include "asuAlloc.h"
#include "tdmLib.h"
#include "duiClasses.hh"
#include "dui_globals.h"
//:----------------------------------------------- PROTOTYPES         --
#include "sutLib.h"

//:=============================================== CLASS              ==
// duiDispatcher

//:----------------------------------------------- CTORS & DTOR       --
duiDispatcher:: duiDispatcher(const char * name)
		: tdmFactory()
		, socFactory()
		, socObject(name, "duiDispatcher") {
//- normal socObject stuff
   myPtr = (SOC_PTR_T)this;
//- normal object factory stuff
   lock(TRUE);

//- specific to DUI
   myCwd=NULL;
   myCwd = (char*)ASUALLOC(strlen(name) +2);
   strcpy(myCwd,"/");
   strcat(myCwd,name);
   if( !dsNewDataset(&pDSroot, (char*)name, DSET_DIM) ){
      dsPerror("duiDispatcher -- Error creating root dataset");
   }
   IDREF_T id;
   if( soc->idObject(myCwd,"tdmDataset",id) ){
      EML_LOG_ERROR(DUPLICATE_OBJECT_NAME);
   }
   myRoot = new tdmDataset(myCwd,pDSroot);
   if( !soc->idObject(myCwd,"tdmDataset",id) ){
      EML_LOG_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   findNode_ds(myCwd,dui_pDScwd);
}

//----------------------------------
duiDispatcher:: ~duiDispatcher() {
// delete[] myRoot;
   ASUFREE(myCwd);
}

//:----------------------------------------------- ATTRIBUTES         --
void duiDispatcher::  cwd (const char * cwd) {
   if(myCwd)ASUFREE(myCwd);
   myCwd = (char*)ASUALLOC(strlen(cwd) +1);
   strcpy(myCwd,cwd);
   findNode_ds(myCwd,dui_pDScwd);
}

//----------------------------------
char * duiDispatcher::  cwd () {
   char *c = (char*)ASUALLOC(strlen(myCwd)+1);
   strcpy(c,myCwd);
   return c;
}

//----------------------------------
tdmDataset* duiDispatcher::  cwdDO () {
    return NULL;	/* NOT YET IMPLEMENTED */
}

//----------------------------------
tdmDataset* duiDispatcher::  rootDO () {
    return myRoot;
}

//:----------------------------------------------- PUB FUNCTIONS      --
STAFCV_T duiDispatcher:: mkTable (const char * filePath
		, const char * spec, long rows) {

   DS_DATASET_T *pDSbase=NULL, *pDSnew=NULL;
   char* pData=NULL;

   if( !findNode_ds(dui_dirof(dui_pathof(myCwd,filePath)), pDSbase)
   ||  !dsAddTable(pDSbase,dui_notdirof(filePath),(char*)spec,rows
		,&pData)
   ){
      EML_ERROR(CANT_CREATE_OBJECT);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: cd (const char * dirPath) {

   DS_DATASET_T *pDS=NULL;
   bool_t result;

   if( !findNode_ds(dirPath,pDS)
   ||  !dsIsDataset(&result,pDS)
   ||  !result
   ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }

   char* newPath = (char*)dui_pathof(myCwd,dirPath);
   char* p=NULL;
   if( NULL == strstr(newPath,p=myRoot->name()) ){
      EML_ERROR(INVALID_DATASET);
   }
   ASUFREE(p);
   cwd(newPath);
   ASUFREE(newPath);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: cp (const char * fromPath
		, const char * toPath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T duiDispatcher:: ls (const char * path, char *& result) {

   DS_DATASET_T *pDS;
   bool_t isTable, isDataset;
   char* errormessage = "*** No such DUI table or directory ***";

   if( !findNode_ds(path,pDS) ){
      result = (char*)ASUALLOC(strlen(errormessage) +1);
      strcpy(result,errormessage);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   if( !dsIsTable(&isTable, pDS)
   ||  !dsIsDataset(&isDataset, pDS)
   ||  !(isTable || isDataset)
   ){
      dsPerror("invalid DSL type");
   }
   if( isTable ) dui_ls_l_Table(pDS,result);
   if( isDataset ) dui_ls_l_Dataset(pDS,result);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: mkdir (const char * dirPath) {

   DS_DATASET_T *pDSbase=NULL, *pDSnew=NULL;
   bool_t isDataset;

   char* c=cvtRelAbs(dirPath);
   char* bName=dui_dirof(c);
   char* nName=dui_notdirof(c);
   ASUFREE(c);

   if( !findNode_ds(bName, pDSbase)
   ||  !dsAddDataset(pDSbase,nName,DSET_DIM,NULL)
   ||  !dsFindEntry(&pDSnew, pDSbase, nName)
   ||  !dsIsDataset(&isDataset,pDSnew)
   ||  !isDataset
   ){
      ASUFREE(bName); ASUFREE(nName);
      EML_ERROR(DSL_ERROR);
   }
   ASUFREE(bName); ASUFREE(nName);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: mv (const char * fromPath
		, const char * toPath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T duiDispatcher:: pwd (char *& result) {
   result = (char*)ASUALLOC(strlen(myCwd) +1);
   strcpy(result,myCwd);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: rm (const char * filePath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T duiDispatcher:: rmdir (const char * dirPath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
char * duiDispatcher:: cvtRelAbs (const char * relPath) {
   char *absPath=NULL;
   if( relPath == NULL ){
      return absPath;	// bad relPath
   }
   char *c=cwd();
   absPath = (char*)dui_pathof(c,relPath);
   if(c)ASUFREE(c);
   return absPath;
}

//**********************************************************************
// Over-ride tdmFactory methods
//----------------------------------
// Over-ride tdmFactory methods

//----------------------------------
STAFCV_T duiDispatcher:: findDataset (const char * dirPath
		, tdmDataset*& dataset) {

   DS_DATASET_T* pDS=NULL;
   char *fullPath=NULL;

   if( !(fullPath = cvtRelAbs(dirPath)) ){
	 EML_ERROR(INVALID_AHS_SPEC);
   }

   if( !tdmFactory::findDataset(fullPath,dataset) ){
      if( !findNode_ds(fullPath,pDS)
      ||  !createDataset(fullPath,pDS,dataset)
      ){
	 dataset = NULL;
	 EML_ERROR(OBJECT_NOT_FOUND);
      }
   }
   ASUFREE(fullPath);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: findTable (const char * filePath
		, tdmTable*& table) {

   DS_DATASET_T* pDS=NULL;
   char *fullPath=NULL;

   if( !(fullPath = cvtRelAbs(filePath)) ){
	 EML_ERROR(INVALID_AHS_SPEC);
   }

   if( !tdmFactory::findTable(fullPath,table) ){
      if( !findNode_ds(fullPath,pDS)
      ||  !createTable(fullPath,pDS,table)
      ){
	 table = NULL;
	 ASUFREE(fullPath);
	 EML_ERROR(OBJECT_NOT_FOUND);
      }
   }
   ASUFREE(fullPath);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: newDataset (const char * name, long setDim){
   IDREF_T id;
   if( soc->idObject(name,"tdmDataset",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   tdmDataset* p;
   if( !mkdir(name)
   ||  !findDataset(name,p) ){
      EML_ERROR(CANT_CREATE_OBJECT);
   }
   if( !soc->idObject(name,"tdmDataset",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiDispatcher:: newTable (const char * name, const char * spec
		, long rows){
   IDREF_T id;
   if( soc->idObject(name,"tdmTable",id) ){
      EML_ERROR(DUPLICATE_OBJECT_NAME);
   }
   tdmTable* p;
   if( !mkTable(name,spec,rows)
   ||  !findTable(name,p) ){
      EML_ERROR(CANT_CREATE_OBJECT);
   }
   if( !soc->idObject(name,"tdmTable",id) ){
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   EML_SUCCESS(STAFCV_OK);

}

//:----------------------------------------------- PRIV FUNCTIONS     --
STAFCV_T duiDispatcher:: findNode_ds (const char * path
		, DS_DATASET_T*& pNode) {
   char* fullPath;
   if( !(fullPath = cvtRelAbs(path))
   ||  !duiFindDS(pNode,pDSroot,fullPath)
   ){
      ASUFREE(fullPath);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   ASUFREE(fullPath);
   EML_SUCCESS(STAFCV_OK);
}

// ---------------------------------------------------------------------

