//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:        duiClasses.C
//:DESCRIPTION: Dataset Unix-like Interface Classes
//:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:        -- STILL IN DEVELOPMENT --
//:HISTORY:     08dec97-v031b-cet- fix ctor bug
//:HISTORY:     17nov97-v031a-cet,hjw- add du,df
//:HISTORY:     14nov97-v030c-cet- Rename ln to lnmv and use for ln
//:HISTORY:     03sep97-v030b-cet- merge CET & HJW versions (minimal)
//:HISTORY:     22Jul97-v030a-hjw- add rm,rmdir,ln,cp,mv
//:HISTORY:     13may97-v020a-cet- lock '/dui'
//:HISTORY:     23dec96-v010a-cet- OLD_DSL -> NEW_DSL default
//:HISTORY:     01feb96-v001a-cet- make more like tdmFactory
//:HISTORY:     08dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:----------------------------------------------- DEFINES           --
#define MAXPATH 123
#define PP printf(
//:----------------------------------------------- INCLUDES           --
#include <string.h>
#include <malloc.h>
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
   preciousList = NULL;

#ifndef OLD_DSL
   if( !dsNewDataset(&pDSroot, (char*)name) ){
#else   /*OLD_DSL*/
   if( !dsNewDataset(&pDSroot, (char*)name, DSET_DIM) ){
#endif  /*OLD_DSL*/
      EML_WARNING(DUI-NO_ROOT); // v030b
   }
   IDREF_T id;
   if( soc->idObject(myCwd,"tdmDataset",id) ){
      EML_WARNING(DUPLICATE_OBJECT_NAME);
   }
   else { // v031b
      myRoot = new tdmDataset(myCwd,pDSroot);
   }
   if( !soc->idObject(myCwd,"tdmDataset",id) ){
      EML_WARNING(OBJECT_NOT_FOUND);
   }
   addEntry(id);
   findNode_ds(myCwd,dui_pDScwd);

//- Lock '/dui' dataset 
   socObject *s=soc->getObject(id);
   s->lock(0);
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
      EML_CONTEXT("ERROR: '%s' is not a existent _directory_.\n",dirPath);
      EML_ERROR(OBJECT_NOT_FOUND);
   }

   char* newPath = (char*)dui_pathof(myCwd,dirPath);
   if( NULL == strstr(newPath,myRoot->Name()) ){
     if(newPath) FREE(newPath);  /*fix memory leaks -phenix*/
      EML_ERROR(INVALID_DATASET);
   }
   cwd(newPath);
   FREE(newPath);
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiFactory:: df (char *markerString) {
  static long previous = -123;
  char buf1[40],buf2[40];
  long thisTime;
#ifndef WIN32
  struct mallinfo qq;
  qq=mallinfo();
  thisTime=qq.usmblks+qq.uordblks;
  // Do we need to delete qq?     No.  hjw 4/30/98.
#else
//          int usmblks;    /* space in small blocks in use */
//          int uordblks;   /* space in ordinary blocks in use */

  thisTime = 0;
#endif /* WIN32 */
  if(previous>=0) {
    duiSprinfWithCommas(buf1,(long)(thisTime));
    duiSprinfWithCommas(buf2,(long)(thisTime-previous));
    printf("%s alloc, inc=%s (%s)\n",buf1,buf2,markerString);
  } else {
    duiSprinfWithCommas(buf1,(long)(thisTime));
    if(!markerString) { 
      printf("Crash imminent. Line %d of %s. Exiting...\n",__LINE__,__FILE__); 
      exit(2);
    }
    printf("%s Bytes of memory allocated (%s)\n",buf1,markerString);
  }
  previous=thisTime;
  EML_SUCCESS(STAFCV_OK);
}
//----------------------------------
#define DUI_PATH_SIZE 150
STAFCV_T duiFactory:: duRecurse (char *path,int indent,DS_DATASET_T *pDS,
      long minsize,int control) {
  bool_t isDataset; DS_DATASET_T *pDS2; const char *dsName;
  static callCnt=0;
  int newlen,ii; const char *dname; char path2[DUI_PATH_SIZE+1],buf[17];
  size_t iEntry,numEntries,rowsize,nrows;
  indent++; indent++; path2[0]=0;
  if(!dsIsDataset(&isDataset,pDS)) EML_ERROR(DATASET_NOT_FOUND);
  if(isDataset) {
    if(!dsDatasetName(&dsName,pDS)) EML_ERROR(CANT_GET_NAME_OF_DIR);
    if(strlen(dsName)+strlen(path)+1>DUI_PATH_SIZE) EML_ERROR(PATH_TOO_BIG);
    sprintf(path2,"%s/%s",path,dsName);
  } else {
    if(!dsTableName(&dsName,pDS)) EML_ERROR(CANT_GET_NAME_OF_TBL);
  }
  if(isDataset) {
    if(!dsDatasetEntryCount(&numEntries,pDS)) EML_ERROR(DATASET_ERROR);
    if(!dsDatasetName(&dname,pDS)) EML_ERROR(DATASET_ERROR);
    if(control==0) {
      printf("%s/%s ",path,dname);
      for(ii=57-strlen(path)-strlen(dname);ii>=0;ii--) {
        printf("%c",(callCnt%3==0)?'-':' ');
      }
      printf(" directory\n"); 
    }
    callCnt++;
    for(iEntry=0;iEntry<numEntries;iEntry++) {
      if(!dsDatasetEntry(&pDS2,pDS,iEntry)) EML_ERROR(DATASET_ERROR);
      if(duRecurse(path2,indent,pDS2,minsize,control)!=STAFCV_OK) 
          EML_ERROR(DATASET_ERROR);
    }
  } else { /* is a table */
    if(!dsTableRowSize(&rowsize,pDS)) EML_ERROR(DATASET_ERROR);
    if(!dsTableMaxRowCount(&nrows,pDS)) EML_ERROR(DATASET_ERROR);
    if( control==0 && (long)(nrows*rowsize)>=minsize ) {
      printf("%s/%s ",path,dsName);
      duiSprinfWithCommas(buf,(long)(nrows*rowsize));
      for(ii=56-strlen(buf)-strlen(path)-strlen(dsName);ii>=0;ii--) {
        printf("%c",(callCnt%3==0)?'-':' ');
      }
      printf(" %s bytes  %6d rows\n",buf,nrows); callCnt++;
      totBytes+=nrows*rowsize;
    }
    if(control==1) {
      if(!preciousList) { printf("Error 66u. Crash imminent.\n"); exit(2); }
      newlen=strlen(path)+strlen(dsName)+3; /*  /  \n  null */
      preciousList=(char*)REALLOC(preciousList,strlen(preciousList)+newlen);
      strcat(preciousList,path);   strcat(preciousList,"/");
      strcat(preciousList,dsName); strcat(preciousList,"\n");
    }
    if(control==2) {  /* exact copy of control==1, except different list */
      if(!current_list) { printf("Error 66u. Crash imminent.\n"); exit(2); }
      newlen=strlen(path)+strlen(dsName)+3;  /*  /  \n  null */
      current_list=(char*)REALLOC(current_list,strlen(current_list)+newlen);
      strcat(current_list,path);   strcat(current_list,"/");
      strcat(current_list,dsName); strcat(current_list,"\n");
    }
  }
  EML_SUCCESS(STAFCV_OK);
}
//----------------------------------
STAFCV_T duiFactory:: rm_nonprecious () {
  DS_DATASET_T *pDS=NULL;
  char *aPreciousTable,*aCurrentTable,isPrecious;
  int ii;

  current_list=(char*)MALLOC(1); current_list[0]=0;

  if(!findNode_ds("/dui",pDS)) EML_ERROR(OBJECT_NOT_FOUND);

  duRecurse("",0,pDS,0,2);   /* build current_list */

  aCurrentTable=strtok(current_list,"\n");
  while(aCurrentTable) {
    isPrecious=0;
    for(ii=0;;ii++) {
      aPreciousTable=strntok(preciousList,"\n",ii);
      if(!aPreciousTable) break;
      if(!strcmp(aCurrentTable,aPreciousTable)) { isPrecious=7; break; }
    }
    if(isPrecious) {
      printf("%20s %s\n","   precious,   save",aCurrentTable);
    } else {
      printf("%20s %s\n","nonprecious, remove",aCurrentTable);
      if(!rm(aCurrentTable)) EML_ERROR(REMOVAL_FAILED);
    }
    aCurrentTable=strtok(NULL,"\n");
  }
  FREE(current_list);
  EML_SUCCESS(STAFCV_OK);
}
STAFCV_T duiFactory:: precious () {
  DS_DATASET_T *pDS=NULL;
  if(preciousList) FREE(preciousList);
  preciousList=(char*)MALLOC(1);
  preciousList[0]=0;
  if(!findNode_ds("/dui",pDS)) EML_ERROR(OBJECT_NOT_FOUND);
  duRecurse("",0,pDS,0,1);
  printf("Precious files:\n");
  fputs(preciousList,stdout);
  EML_SUCCESS(STAFCV_OK);
}
void duiFactory:: duiSprinfWithCommas(char *out,long in) {
  char *p,buf[77];
  int ii,len,leadoff;
  if(in<=0) { sprintf(out,"%ld",in); return; }
  sprintf(buf,"%ld",in); len=strlen(buf); p=out;
  leadoff=len%3+98;
  for(ii=0;ii<len;ii++) {
    *(p++)=buf[ii]; if(!buf[ii+1]) break;
    if((leadoff--)%3==0) *(p++)=',';
  }
  *(p++)=0;
}
//----------------------------------
STAFCV_T duiFactory:: du (const char * dirPath,long minsize) {

   DS_DATASET_T *pDS=NULL; int i; char buf[50];

   if( !findNode_ds(dirPath,pDS)) {
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   char* path2 = (char*)dui_pathof(myCwd,dirPath);
   if( NULL == strstr(path2,myRoot->Name()) ){
      EML_ERROR(INVALID_DATASET);
   }
   for(i=strlen(path2)-1;i>=0;i--) { if(path2[i]=='/') { path2[i]=0; break; } }
   totBytes=0;
   if(duRecurse(path2,0,pDS,minsize,0)!=STAFCV_OK) {
     EML_ERROR(CANNOT_TRAVERSE_TREE);
   }
   FREE(path2); duiSprinfWithCommas(buf,(long)totBytes);
   printf("                                     Total bytes %11s\n",buf);
   if(minsize>0) {
      printf("Only tables of size >= %ld bytes are listed above.\n",minsize);
   }
   EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
#define BUF 256
STAFCV_T duiFactory:: lnmv (char unlinkSrc, const char * fromPath , 
       const char * tgtDir) {      // also called for mv (with unlinkSrc true)

  DS_DATASET_T *from=NULL,*to=NULL;
  bool_t isDataset;

  if(!findNode_ds(fromPath,from))  EML_ERROR(SRC_NOT_FOUND);

  if(!findNode_ds(tgtDir,to))     EML_ERROR(SECOND_PARAM_MUST_BE_DIR);
  if(!dsIsDataset(&isDataset,to)) EML_ERROR(INVALID_TGT_DIRECTORY);
  if(!isDataset)                  EML_ERROR(TGT_MUST_BE_DIR_NOT_TABLE_NAME);
  
  if(!dsLinkAcyclic(to,from)) EML_ERROR(LINK_FAILED);

  if(unlinkSrc) unlinkAndMaybeFreeMemory(FALSE,fromPath);

  EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
STAFCV_T duiFactory:: ln (const char * fromPath
		, const char * toPath) {
   return lnmv(FALSE,fromPath,toPath);
}

//----------------------------------
STAFCV_T duiFactory:: append (const char * fromPath
		, const char * targPath) {
  DS_DATASET_T *from=NULL,*targ=NULL;
  size_t nbytes,nokFrom,nmaxFrom,nmaxTarg,nokTarg,offset,sizeTarg,sizeFrom;
  const char *fromSpec,*targSpec; char *fromData,*targData;

  if(!findNode_ds(fromPath,from)) EML_ERROR(SRC_NOT_FOUND);
  if(!dsTableRowCount(&nokFrom,from)) EML_ERROR(SRC_TABLE_NOT_FOUND);
  if(!dsTableMaxRowCount(&nmaxFrom,from)) EML_ERROR(SRC_TABLE_NOT_FOUND);
  if(!dsTableTypeSpecifier(&fromSpec,from)) EML_ERROR(SRC_SPECS_NOT_FOUND);
  if(!dsTableRowSize(&sizeFrom,from)) EML_ERROR(SRC_SIZE_NOT_FOUND);

  if(!findNode_ds(targPath,targ)) EML_ERROR(TGT_NOT_FOUND);
  if(!dsTableRowCount(&nokTarg,targ)) EML_ERROR(TGT_TABLE_NOT_FOUND);
  if(!dsTableMaxRowCount(&nmaxTarg,targ)) EML_ERROR(TGT_TABLE_NOT_FOUND);
  if(!dsTableTypeSpecifier(&targSpec,targ)) EML_ERROR(TGT_SPECS_NOT_FOUND);
  if(!dsTableRowSize(&sizeTarg,targ)) EML_ERROR(TGT_SIZE_NOT_FOUND);

  if(strcmp(targSpec,fromSpec)) EML_ERROR(SRC_AND_TGT_DIFFERENT_TYPES);
  if(sizeFrom!=sizeTarg) EML_ERROR(SRC_AND_TGT_DIFFERENT_SIZES);

  if(!dsReallocTable(targ,nokFrom+nokTarg)) EML_ERROR(TGT_REALLOC_FAILURE);
  if(!dsSetTableRowCount(targ,nokFrom+nokTarg)) EML_ERROR(TGT_SETROW_FAILURE);

  if(!dsTableDataAddress(&targData,targ)) EML_ERROR(TGT_DATA_ADDR_NOT_FOUND);
  if(!dsTableDataAddress(&fromData,from)) EML_ERROR(SRC_DATA_ADDR_NOT_FOUND);
  nbytes=nokFrom*sizeFrom;
  offset=nokTarg*sizeTarg;
  memcpy(targData+offset,fromData,nbytes);

  EML_SUCCESS(STAFCV_OK);
}
STAFCV_T duiFactory:: cp (const char * fromPath
		, const char * toPath) {
  DS_DATASET_T *from=NULL,*to=NULL,*pDS;
  char *toPathExtended,*slash;
  long nmaxLong;
  const char *spec;
  size_t nmax,nok,nbytes,rowSize;
  char *pDataTo,*pDataFrom;
  int ii;
  bool_t isDirectory;

  // If toPath is a directory, add the table name to the end of it.
  if(findNode_ds(toPath,pDS)) {
    if(!dsIsDataset(&isDirectory,pDS)) EML_ERROR(TGT_NOT_FOUND);
    if(!isDirectory) EML_ERROR(TGT_ALREADY_EXISTS);
    for(ii=strlen(fromPath)-1;ii>=0;ii--) if(fromPath[ii]=='/') break;
    ii++;
    toPathExtended=(char*)MALLOC(strlen(toPath)+strlen(fromPath+ii)+3);
    if(toPath[strlen(toPath)-1]=='/') slash=""; else slash="/";
    sprintf(toPathExtended,"%s%s%s",toPath,slash,fromPath+ii);
  } else {
    toPathExtended=(char*)MALLOC(strlen(toPath)+1);
    strcpy(toPathExtended,toPath);
  }
  /*fix memory leaks -akio*/
  if(!findNode_ds(fromPath,from)){
    FREE(toPathExtended);                 EML_ERROR(SRC_NOT_FOUND);}
  if(!dsTableRowCount(&nok,from)){
    FREE(toPathExtended);                 EML_ERROR(TABLE_NOT_FOUND);}
  if(!dsTableMaxRowCount(&nmax,from)){
    FREE(toPathExtended);                 EML_ERROR(TABLE_NOT_FOUND);}
  if(!dsTableTypeSpecifier(&spec,from)){
    FREE(toPathExtended);                 EML_ERROR(SPECS_NOT_FOUND);}
  nmaxLong=nmax;
  newTable(toPathExtended,spec,nmaxLong);
  if(!findNode_ds(toPathExtended,to)){
    FREE(toPathExtended);                 EML_ERROR(TGT_NOT_CREATED);}
  if(!dsSetTableRowCount(to,nok)){
    FREE(toPathExtended);                 EML_ERROR(NROW_NOT_SET);}
  if(!dsAllocTables(to)){
    FREE(toPathExtended);                 EML_ERROR(TBL_MEMORY_NOT_ALLOCATED);}
  if(!dsTableDataAddress(&pDataTo,to)){
    FREE(toPathExtended);                 EML_ERROR(TGT_DATA_ADDR_NOT_FOUND);}
  if(!dsTableDataAddress(&pDataFrom,from)){
    FREE(toPathExtended);                 EML_ERROR(SRC_DATA_ADDR_NOT_FOUND);}
  if(!dsTableRowSize(&rowSize,to)){
    FREE(toPathExtended);                 EML_ERROR(ROW_SIZE_NOT_FOUND);}
  nbytes=nok*rowSize;
  memcpy(pDataTo,pDataFrom,nbytes);

  FREE(toPathExtended);

  EML_SUCCESS(STAFCV_OK);
}

//----------------------------------
char * duiFactory:: ls (const char * path) {

   DS_DATASET_T *pDS;
   bool_t isTable, isDataset;

   char *result;
   char* errormessage = "*** No such DUI table or directory ***";

   if( !findNode_ds(path,pDS) ){
      result = (char*)MALLOC(strlen(errormessage) +1);
      strcpy(result,errormessage);
/*      EML_ERROR(OBJECT_NOT_FOUND);  fix memory leak -akio */
      return result; /*fix memory leak -akio*/
   }
   if( !dsIsTable(&isTable, pDS)
   ||  !dsIsDataset(&isDataset, pDS)
   ||  !(isTable || isDataset)
   ){
      dsPerror("invalid DSL type");
   }

   result=(char*)MALLOC(5000);   // 24jul97 hjw
   result[0]=0;                  // 24jul97 hjw, initialization.

   if( isTable ) dui_ls_l_Table(pDS,result);
   if( isDataset ) dui_ls_l_Dataset(pDS,result);
/*-16jul97-   EML_SUCCESS(STAFCV_OK); -*/
   return result;
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
   return lnmv(TRUE,fromPath,toPath);
}

//----------------------------------
char * duiFactory:: pwd () {
   char * result = (char*)MALLOC(strlen(myCwd) +1);
   strcpy(result,myCwd);
/*-16jul97-   EML_SUCCESS(STAFCV_OK); -*/
   return result;
}

//----------------------------------
STAFCV_T duiFactory:: rm ( const char * filePath) {

  DS_DATASET_T *pp=NULL;
  bool_t isDir;

  if(!findNode_ds(filePath,pp)) {
    EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",filePath);
    EML_ERROR(DIR_NOT_FOUND);
  }
  if(!dsIsDataset(&isDir,pp))  EML_ERROR(DATASET_NOT_FOUND);
  if(isDir) {
    EML_CONTEXT("ERROR: '%s' is dir.  Use 'rmdir' to remove.\n",filePath);
    EML_ERROR(USE_RMDIR_FOR_DIRS_NOT_RM);
  }
  if(unlinkAndMaybeFreeMemory(TRUE,filePath)!=STAFCV_OK) {
    EML_ERROR(REMOVAL_FAILED);
  }
  EML_SUCCESS(STAFCV_OK);
}
STAFCV_T duiFactory:: unlinkAndMaybeFreeMemory (char freeMemory, 
        const char * filePath) {
  // Arg freeMemory is false when
  // fnct is called from mv (move).  In this case we do only the
  // unlink.  Arg freeMemory is true when fnct is called
  // as part of a bona fide rm (remove).

  DS_DATASET_T *parentOfFrom=NULL; 
  int ii;
  char* fullPath,tmp[MAXPATH+1];
  DS_DATASET_T *from=NULL;
  size_t numLinks;
  bool_t isTable;

  if(!findNode_ds(filePath,from))  {
    EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",filePath);
    EML_ERROR(TABLE_NOT_FOUND);
  }

  if(!dsIsTable(&isTable,from)) EML_ERROR(DATASET_NOT_FOUND);
  if(!dsRefcount(&numLinks,from)) EML_ERROR(DATASET_NOT_FOUND);

  fullPath=(char*)dui_pathof(myCwd,filePath);
  if(!fullPath) EML_ERROR(FAIL_CREATE_FULL_PATH_OF_SRC);
  if(strlen(fullPath)>MAXPATH) {
    FREE(fullPath);  /*fix memory leak -akio/phenix*/
    EML_ERROR(FULL_PATH_OF_SRC_TOO_LONG);
  }
  strcpy(tmp,fullPath);
  FREE(fullPath);  /*fix memory leak -akio*/
  for(ii=strlen(tmp)-1;ii>=0;ii--) if(tmp[ii]=='/') break; tmp[ii]=0;
  if(!strstr(tmp,"/")) EML_ERROR(NO_PARENT_DIR_FOR_SRC);
  if(!findNode_ds(tmp,parentOfFrom)) EML_ERROR(PARENT_DIR_OF_SRC_NOT_FOUND);
  if(!dsUnlink(parentOfFrom,from)) EML_ERROR(UNLINK_OF_SRC_FAILED);
  if(freeMemory&&numLinks<2) {
    if(!dsFreeDataset(from)) EML_ERROR(MEMORY_NOT_FREED);
    /*- 03dec97-cet-Dave Morrison's patch -*/
    tdm->deleteTable(filePath);
  }

  EML_SUCCESS(STAFCV_OK);

} // filePath

//----------------------------------
STAFCV_T duiFactory:: rmdir (const char * dirPath) { // www

  DS_DATASET_T *dsp=NULL,*pp=NULL;
  size_t ii,nentry;
  const char *theName=NULL;char *newPath=NULL, *fullpath=NULL;
  bool_t isDir;

  if(!findNode_ds(dirPath,pp)) {
    EML_CONTEXT("ERROR: Are you sure you have a '%s'?\n",dirPath);
    EML_ERROR(DIR_NOT_FOUND);
  }
  if(!dsIsDataset(&isDir,pp))  EML_ERROR(DATASET_NOT_FOUND);
  if(!isDir)                   EML_ERROR(USE_RM_FOR_TABLES_NOT_RMDIR);
  if(!dsDatasetEntryCount(&nentry,pp)) EML_ERROR(DATASET_NOT_FOUND);
  fullpath=dui_pathof(myCwd,dirPath);

  for(ii=0;ii<nentry;ii++) {
    if(!dsDatasetEntry(&dsp,pp,ii)){
      FREE(fullpath); /*fix memory leak -akio*/
      EML_ERROR(ENTRY_NOT_FOUND);
    }
    if(!dsIsDataset(&isDir,dsp)){
      FREE(fullpath); /*fix memory leak -akio*/
      EML_ERROR(DATASET_NOT_FOUND);
    }
    if(isDir) {
      if(!dsDatasetName(&theName,dsp)) EML_ERROR(DATASET_NOT_FOUND);
      newPath=(char*)MALLOC((size_t)(strlen(fullpath)+strlen(theName)+2));
      if(!newPath) {
 	FREE(fullpath);  
 	EML_ERROR(MEM_ALLOC_FAILED);
      }	
      sprintf(newPath,"%s/%s",fullpath,theName);
      rmdir(newPath);
      FREE(newPath);
    } else {
      if(!dsTableName(&theName,dsp)) EML_ERROR(DATASET_NOT_FOUND);
      newPath=(char*)MALLOC((size_t)(strlen(fullpath)+strlen(theName)+2));
      if(!newPath) {
 	FREE(fullpath);
 	EML_ERROR(MEM_ALLOC_FAILED);
      }	
      sprintf(newPath,"%s/%s",fullpath,theName);
      unlinkAndMaybeFreeMemory(TRUE,newPath);
      FREE(newPath);
    }
  }
  
  // We have eliminated all sub dirs and their tables, so we can now
  // elim the dir itself.
  unlinkAndMaybeFreeMemory(TRUE,fullpath);

  FREE(fullpath);
  
  EML_SUCCESS(STAFCV_OK);
} // dirPath fullPath fullpath

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
         FREE(fullPath); /*fix memory leak -akio*/
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
tdmDataset* 
duiFactory::newDataset (const char * name, long setDim)
{
  //Just to hush pedantic compilers
  static void *ps = &setDim;

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
   char *p1=NULL;   /*fix memory leak -akio/phenix*/
   char *p2=NULL;   /*fix memory leak -akio/phenix*/
   char *p3=NULL;   /*fix memory leak -akio/phenix*/

   if( soc->idObject(name,"tdmTable",id) ){
      // EML_ERROR(DUPLICATE_OBJECT_NAME);
      return NULL;
   }
   tdmTable* p=NULL;
   DS_DATASET_T *pDSbase=NULL;
   char* pData=NULL;
   if( NULL == (p = findTable(name)) ){	//- create object from pointer
      					//- ... or create pointer
     if( !findNode_ds(p1=dui_dirof(p2=dui_pathof(myCwd,name)), pDSbase)
	 ||  !dsAddTable(pDSbase,(p3=dui_notdirof(name)),(char*)spec,rows,&pData)
	 ||  NULL == (p = findTable(name))	//- create object from pointer
	 ){                                      /*fix memory leak -akio/phenix*/
       // EML_ERROR(CANT_CREATE_OBJECT);
       if(p1) FREE(p1);  /*fix memory leak -akio/phenix*/
       if(p2) FREE(p2);  /*fix memory leak -akio/phenix*/
       if(p3) FREE(p3);  /*fix memory leak -akio/phenix*/
       return NULL; 
      }
   }
   if(p1) FREE(p1);  /*fix memory leak -akio/phenix*/
   if(p2) FREE(p2);  /*fix memory leak -akio/phenix*/
   if(p3) FREE(p3);  /*fix memory leak -akio/phenix*/
   addEntry(id);
   // EML_SUCCESS(STAFCV_OK);
   return p;
}

//:----------------------------------------------- PRIV FUNCTIONS     --
STAFCV_T duiFactory:: findNode_ds (const char * path
		, DS_DATASET_T*& pNode) {
   char* fullPath=NULL,thereIsAnError=0;
   if( !(fullPath = cvtRelAbs(path)) ) thereIsAnError=7;
   if( !thereIsAnError ) {
      if( !duiFindDS(pNode,pDSroot,fullPath) ) thereIsAnError=7;
   }
   if( thereIsAnError ){
      FREE(fullPath);
      EML_CONTEXT("ERROR: I can't find '%s'.\n",path);
      EML_ERROR(OBJECT_NOT_FOUND);
   }
   FREE(fullPath);
   EML_SUCCESS(STAFCV_OK);
}

// ---------------------------------------------------------------------

