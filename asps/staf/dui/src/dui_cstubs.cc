/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         dui_kam.c
*:DESCRIPTION:  C KUIP Action Modules for Error & Message Logger
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      08dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "asuAlloc.h"
#include "emlLib.h"
#include "dui_macros.h"
#include "dui_types.h"
#include "dui_globals.h"

STAFCV_T
dui_cd(char* path)
{
   if( !dui->cd(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_df(char *markerString)
{
   if( !dui->df(markerString) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_rm_nonprecious()
{
   if( !dui->rm_nonprecious() ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_precious()
{
   if( !dui->precious() ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_du()
{
   if( !dui->du("/dui",0) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_ln(char* fromPath, char* toPath)
{
   if(strstr(fromPath,".")) {
     EML_CONTEXT("ERROR: don't use dots (.)\n");
     EML_FAILURE(DONT_USE_DOTS_IN_TABLE_NAMES);
   }
   if(strstr(  toPath,".")) {
     EML_CONTEXT("ERROR: don't use dots (.)\n");
     EML_FAILURE(DONT_USE_DOTS_IN_TABLE_NAMES);
   }
   if( !dui->ln(fromPath,toPath) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_append(char* fromPath, char* toPath)
{
   if(strstr(fromPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if(strstr(  toPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->append(fromPath,toPath) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_cp(char* fromPath, char* toPath)
{
   if(strstr(fromPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if(strstr(  toPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->cp(fromPath,toPath) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_ls(char* path)
{
   char *result;   // 24jul97 hjw removed malloc; result was reset a 
                   // few lines below.
   // strncpy(result,"",1);     24jul97 hjw, must init in dui->ls() instead 

   if( NULL == (result = dui->ls(path)) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   printf("DUI:\tListing = ...\n%s\n.\n",result);
   FREE(result);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_mkdir(char* path)
{
   if( !dui->mkdir(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_mv(char* fromPath, char* toPath)
{
   if(strstr(fromPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if(strstr(  toPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->mv(fromPath,toPath) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_pwd()
{
   char* result=NULL;

   if( NULL == (result = dui->pwd()) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   printf("DUI:\tCurrent Working Directory = (%s) \n",result);
   FREE(result);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_rm(char* path)
{
   if(strstr(path,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->rm(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
dui_rmdir(char* path)
{
   if(strstr(path,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->rmdir(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

