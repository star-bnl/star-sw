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
      EML_ERROR(KAM_METHOD_FAILURE);
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
   if( !dui->cp(fromPath,toPath) ){
      EML_ERROR(KAM_METHOD_FAILURE);
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

	STAFCV_T status = dui_ls(path);
}
STAFCV_T dui_ls(char* path)
{
   char * result = (char*)ASUALLOC(2048);
   strncpy(result,"",1);
   if( !dui->ls(path, result) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   printf("DUI:\tListing = ...\n%s\n.\n",result);
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
      EML_ERROR(KAM_METHOD_FAILURE);
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
   if( !dui->mv(fromPath,toPath) ){
      EML_ERROR(KAM_METHOD_FAILURE);
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

	STAFCV_T status = dui_pwd();
}
STAFCV_T dui_pwd()
{
   char* result=NULL;

   if( !dui->pwd(result) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   printf("DUI:\tCurrent Working Directory = (%s) \n",result);
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
   if( !dui->rm(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
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
   if( !dui->rmdir(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_dui_table_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* DUI/MKTABLE NAME SPEC ROWCOUNT
*:<---------------------------------------------------------------------
*/
void kam_dui_mktable_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* table name */
   char* spec = ku_gets();      /* table row specifier */
   long rowcount = ku_geti();   /* rows to allocate */

	STAFCV_T status = dui_mktable(name,spec,rowcount);
}
STAFCV_T dui_mktable(char* name,char* spec,long rowcount)
{
   if( !dui->mkTable(name,spec,rowcount) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

