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

#include "kuip.h"
#define KUIP

#include "asuAlloc.h"
#include "emlLib.h"
#include "dui_macros.h"
#include "dui_types.h"
#include "dui_globals.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_cd_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/CD [ PATH ]
*:<---------------------------------------------------------------------
*/
void kam_dui_cd_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

	STAFCV_T status = dui_cd(path);
}
STAFCV_T dui_cd(char* path)
{
   if( !dui->cd(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_df_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/DF
*:<---------------------------------------------------------------------
*/
void kam_dui_df_()
{
        STAFCV_T status = dui_df();
}
STAFCV_T dui_df()
{
   if( !dui->df() ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_du_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/DU
*:<---------------------------------------------------------------------
*/
void kam_dui_du_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
        STAFCV_T status = dui_du();
}
STAFCV_T dui_du()
{
   if( !dui->du("/dui",0) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_ln_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/CP FROM TO
*:<---------------------------------------------------------------------
*/
void kam_dui_ln_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  fromPath = ku_gets();	/* path */
   char*  toPath = ku_gets();	/* path */

	STAFCV_T status = dui_ln( fromPath, toPath);
}
STAFCV_T dui_ln(char* fromPath, char* toPath)
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
void kam_dui_append_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  fromPath = ku_gets();	/* path */
   char*  toPath = ku_gets();	/* path */

	STAFCV_T status = dui_append(fromPath, toPath);
}
STAFCV_T dui_append(char* fromPath, char* toPath)
{
   if(strstr(fromPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if(strstr(  toPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->append(fromPath,toPath) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_cp_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/CP FROM TO
*:<---------------------------------------------------------------------
*/
void kam_dui_cp_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  fromPath = ku_gets();	/* path */
   char*  toPath = ku_gets();	/* path */

	STAFCV_T status = dui_cp(fromPath, toPath);
}
STAFCV_T dui_cp(char* fromPath, char* toPath)
{
   if(strstr(fromPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if(strstr(  toPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->cp(fromPath,toPath) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_ls_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/LS [ PATH ]
*:<---------------------------------------------------------------------
*/
void kam_dui_ls_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

//EML_TRACE(DEBUG);
	STAFCV_T status = dui_ls(path);
}
STAFCV_T dui_ls(char* path)
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

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_mkdir_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/MKDIR PATH
*:<---------------------------------------------------------------------
*/
void kam_dui_mkdir_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

	STAFCV_T status = dui_mkdir(path);
}
STAFCV_T dui_mkdir(char* path)
{
   if( !dui->mkdir(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_mv_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/MV FROM TO
*:<---------------------------------------------------------------------
*/
void kam_dui_mv_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  fromPath = ku_gets();	/* fromPath */
   char*  toPath = ku_gets();	/* toPath */

	STAFCV_T status = dui_mv(fromPath, toPath);
}
STAFCV_T dui_mv(char* fromPath, char* toPath)
{
   if(strstr(fromPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if(strstr(  toPath,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->mv(fromPath,toPath) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_pwd_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/PWD
*:<---------------------------------------------------------------------
*/
void kam_dui_pwd_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

//EML_TRACE(DEBUG);
	STAFCV_T status = dui_pwd();
}
STAFCV_T dui_pwd()
{
   char* result=NULL;

   if( NULL == (result = dui->pwd()) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   printf("DUI:\tCurrent Working Directory = (%s) \n",result);
   FREE(result);
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_rm_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/RM PATH
*:<---------------------------------------------------------------------
*/
void kam_dui_rm_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

	STAFCV_T status = dui_rm(path);
}
STAFCV_T dui_rm(char* path)
{
   if(strstr(path,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->rm(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_rmdir_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/RMDIR PATH
*:<---------------------------------------------------------------------
*/
void kam_dui_rmdir_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

	STAFCV_T status = dui_rmdir(path);
}
STAFCV_T dui_rmdir(char* path)
{
   if(strstr(path,".")) EML_FAILURE(DONT_USE_DOTS);
   if( !dui->rmdir(path) ){
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

