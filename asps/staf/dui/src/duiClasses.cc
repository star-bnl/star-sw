//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        duiClasses.C
//:DESCRIPTION: Dataset Unix-like Interface Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     23dec96-v010a-cet- OLD_DSL -> NEW_DSL default
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

//:#####################################################################
//:=============================================== CLASS              ==
// duiFactory

//:----------------------------------------------- CTORS & DTOR       --
duiFactory:: duiFactory(const char * name)
		: tdmFactory()
		, socFactory()
		, socObject(name, "duiFactory") {
//- normal socObject stuff
   myPtr = (SOC_PTR_T)this;
//- normal object factory stuff
   lock(TRUE);

//- specific to DUI
   myCwd=NULL;
   myCwd = (char*)MALLOC(strlen(name) +2);
   strcpy(myCwd,"/");
   strcat(myCwd,name);
   pDSroot = NULL;

#ifndef OLD_DSL
   if( !dsNewDataset(&pDSroot, (char*)name) ){
#else   /*OLD_DSL*/
   if( !dsNewDataset(&pDSroot, (char*)name, DSET_DIM) ){
#endif  /*OLD_DSL*/
      dsPerror("duiFactory -- Error creating root dataset");
   }
   IDREF_T id;
   if( soc->idObject(myCwd,"tdmDataset",id) ){
      EML_WARNING(DUPLICATE_OBJECT_NAME);
   }
   myRoot = new tdmDataset(myCwd,pDSroot);
   if( !soc->idObject(myCwd,"tdmDataset",id) ){
      EML_WARNING(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   findNode_ds(myCwd,dui_pDScwd);
}

//----------------------------------
duiFactory:: ~duiFactory() {
// delete[] myRoot;
   FREE(myCwd);
}

//:----------------------------------------------- ATTRIBUTES         --
void duiFactory::  cwd (const char * cwd) {
   if(myCwd)FREE(myCwd);
   myCwd = (char*)MALLOC(strlen(cwd) +1);
   strcpy(myCwd,cwd);
   findNode_ds(myCwd,dui_pDScwd);
}

//----------------------------------
char * duiFactory::  cwd () {
   char *c = (char*)MALLOC(strlen(myCwd)+1);
   strcpy(c,myCwd);
   return c;
}

//----------------------------------
tdmDataset* duiFactory::  cwdDO () {
    return NULL;	/* NOT YET IMPLEMENTED */
}

//----------------------------------
tdmDataset* duiFactory::  rootDO () {
    return myRoot;
}

//:----------------------------------------------- PUB FUNCTIONS      --
//- override socObject::implementsInterface
unsigned char duiFactory :: implementsInterface (const char * iface) {
   if( 0 == strcmp("duiFactory",iface)
   ||  tdmFactory::implementsInterface(iface)
   ){ return TRUE; }
   return FALSE;
}

//----------------------------------
STAFCV_T duiFactory:: cd (const char * dirPath) {

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
   FREE(p);
   cwd(newPath);
   FREE(newPath);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiFactory:: cp (const char * fromPath
		, const char * toPath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T duiFactory:: ls (const char * path, char *& result) {

   DS_DATASET_T *pDS;
   bool_t isTable, isDataset;
   char* errormessage = "*** No such DUI table or directory ***";

   if( !findNode_ds(path,pDS) ){
      result = (char*)MALLOC(strlen(errormessage) +1);
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
STAFCV_T duiFactory:: mkdir (const char * dirPath) {

   DS_DATASET_T *pDSbase=NULL, *pDSnew=NULL;
   bool_t isDataset;

   char* c=cvtRelAbs(dirPath);
   char* bName=dui_dirof(c);
   char* nName=dui_notdirof(c);
   FREE(c);

   if( !findNode_ds(bName, pDSbase)
#ifndef OLD_DSL
   ||  !dsNewDataset(&pDSnew, nName)
   ||  !dsLink(pDSbase, pDSnew)
#else   /*OLD_DSL*/
   ||  !dsAddDataset(pDSbase,nName,DSET_DIM,NULL)
   ||  !dsFindEntry(&pDSnew, pDSbase, nName)
#endif  /*OLD_DSL*/
   ||  !dsIsDataset(&isDataset,pDSnew)
   ||  !isDataset
   ){
      FREE(bName); FREE(nName);
      EML_DSPERROR(DSL_ERROR);
   }
   FREE(bName); FREE(nName);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiFactory:: mv (const char * fromPath
		, const char * toPath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T duiFactory:: pwd (char *& result) {
   result = (char*)MALLOC(strlen(myCwd) +1);
   strcpy(result,myCwd);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiFactory:: rm (const char * filePath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
STAFCV_T duiFactory:: rmdir (const char * dirPath) {
   EML_ERROR(NOT_YET_IMPLEMENTED);
}

//----------------------------------
char * duiFactory:: cvtRelAbs (const char * relPath) {
   char *absPath=NULL;
   if( relPath == NULL ){
      return absPath;	// bad relPath
   }
   char *c=cwd();
   absPath = (char*)dui_pathof(c,relPath);
   if(c)FREE(c);
   return absPath;
}

//**********************************************************************
// Over-ride tdmFactory methods

//----------------------------------
tdmDataset* duiFactory:: findDataset (const char * dirPath) {

   DS_DATASET_T* pDS=NULL;
   char *fullPath=NULL;

   if( !(fullPath = cvtRelAbs(dirPath)) ){
	 EML_ERROR(INVALID_AHS_SPEC);
   }

   tdmDataset* dataset = NULL;

   if( NULL == (dataset = tdmFactory::findDataset(fullPath)) ){
      if( !findNode_ds(fullPath,pDS)
      ||  NULL == (dataset = createDataset(fullPath,pDS))
      ){
	 // EML_ERROR(OBJECT_NOT_FOUND);
	 return NULL;
      }
   }
   FREE(fullPath);
   // EML_SUCCESS(STAFCV_OK);
   return dataset;
}

//----------------------------------
tdmTable* duiFactory:: findTable (const char * filePath) {

   DS_DATASET_T* pDS=NULL;
   char *fullPath=NULL;

   if( !(fullPath = cvtRelAbs(filePath)) ){
	 EML_ERROR(INVALID_AHS_SPEC);
   }

   tdmTable* table=NULL;
   if( NULL == (table = tdmFactory::findTable(fullPath)) ){
      if( !findNode_ds(fullPath,pDS)
      ||  NULL == (table = createTable(fullPath,pDS))
      ){
	 FREE(fullPath);
	 // EML_ERROR(OBJECT_NOT_FOUND);
	 return NULL;
      }
   }
   FREE(fullPath);
   // EML_SUCCESS(STAFCV_OK);
   return table;
}

//----------------------------------
tdmDataset* duiFactory:: newDataset (const char * name, long setDim){
   IDREF_T id;
   if( soc->idObject(name,"tdmDataset",id) ){
      // EML_ERROR(DUPLICATE_OBJECT_NAME);
      return NULL;
   }
   tdmDataset* p;
   if( NULL == (p = findDataset(name)) ){//- create object from pointer
      if( !mkdir(name)			//- create pointer
      ||  NULL == (p = findDataset(name))//- create object from pointer
      ){
	 // EML_ERROR(CANT_CREATE_OBJECT);
	 return NULL;
      }
   }
   addEntry(id);
   // EML_SUCCESS(STAFCV_OK);
   return p;
}

//----------------------------------
tdmTable* duiFactory:: newTable (const char * name
		, const char * spec, long rows ){
   IDREF_T id;
   if( soc->idObject(name,"tdmTable",id) ){
      // EML_ERROR(DUPLICATE_OBJECT_NAME);
      return NULL;
   }
   tdmTable* p=NULL;
   DS_DATASET_T *pDSbase=NULL, *pDSnew=NULL;
   char* pData=NULL;
   if( NULL == (p = findTable(name)) ){	//- create object from pointer
      					//- ... or create pointer
      if( !findNode_ds(dui_dirof(dui_pathof(myCwd,name)), pDSbase)
      ||  !dsAddTable(pDSbase,dui_notdirof(name),(char*)spec,rows,&pData)
      ||  NULL == (p = findTable(name))	//- create object from pointer
      ){
	 // EML_ERROR(CANT_CREATE_OBJECT);
	 return NULL;
      }
   }
   addEntry(id);
   // EML_SUCCESS(STAFCV_OK);
   return p;
}

//:----------------------------------------------- PRIV FUNCTIONS     --
STAFCV_T duiFactory:: findNode_ds (const char * path
		, DS_DATASET_T*& pNode) {
   char* fullPath;
   if( !(fullPath = cvtRelAbs(path))
   ||  !duiFindDS(pNode,pDSroot,fullPath)
   ){
      FREE(fullPath);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   FREE(fullPath);
   EML_SUCCESS(STAFCV_OK);
}

// ---------------------------------------------------------------------

