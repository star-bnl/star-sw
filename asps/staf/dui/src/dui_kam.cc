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
void kam_dui_cd_(){kam_dui_cd();}
int kam_dui_cd()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

   if( !dui->cd(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_cp_(){kam_dui_cp();}
int kam_dui_cp()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  fromPath = ku_gets();	/* path */
   char*  toPath = ku_gets();	/* path */

   if( !dui->cp(fromPath,toPath) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_ls_(){kam_dui_ls();}
int kam_dui_ls()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */


   char * result = (char*)ASUALLOC(2048);
   strncpy(result,"",1);
   if( !dui->ls(path, result) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   printf("DUI:\tListing = \n***\n%s\n***\n",result);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_mkdir_(){kam_dui_mkdir();}
int kam_dui_mkdir()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

   if( !dui->mkdir(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_mv_(){kam_dui_mv();}
int kam_dui_mv()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  fromPath = ku_gets();	/* fromPath */
   char*  toPath = ku_gets();	/* toPath */

   if( !dui->mv(fromPath,toPath) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_pwd_(){kam_dui_pwd();}
int kam_dui_pwd()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   char* result=NULL;

   if( !dui->pwd(result) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   printf("DUI:\tCurrent Working Directory = (%s) \n",result);
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_rm_(){kam_dui_rm();}
int kam_dui_rm()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

   if( !dui->rm(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_rmdir_(){kam_dui_rmdir();}
int kam_dui_rmdir()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  path = ku_gets();	/* path */

   if( !dui->rmdir(path) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
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
void kam_dui_mktable_(){kam_dui_mktable();}
int kam_dui_mktable()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char* name = ku_gets();      /* table name */
   char* spec = ku_gets();      /* table row specifier */
   long rowcount = ku_geti();   /* rows to allocate */

   if( !dui->mkTable(name,spec,rowcount) ){
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(NORMAL_SUCCESSFUL_COMPLETION);
}

