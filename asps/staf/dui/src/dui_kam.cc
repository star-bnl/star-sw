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
void 
kam_dui_cd_()
{
   char*  path = ku_gets();	/* path */

   dui_cd(path);
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
void 
kam_dui_df_()
{
  char*  markerString = ku_gets();
  dui_df(markerString);
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
void 
kam_dui_du_()
{
  dui_du();
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
void 
kam_dui_ln_()
{
   char*  fromPath = ku_gets();	/* path */
   char*  toPath = ku_gets();	/* path */

   dui_ln( fromPath, toPath);
}

void 
kam_dui_append_()
{
   char*  fromPath = ku_gets();	/* path */
   char*  toPath = ku_gets();	/* path */

   dui_append(fromPath, toPath);
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
void 
kam_dui_cp_()
{
   char*  fromPath = ku_gets();	/* path */
   char*  toPath = ku_gets();	/* path */

   dui_cp(fromPath, toPath);
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
void 
kam_dui_ls_()
{
   char*  path = ku_gets();	/* path */

   dui_ls(path);
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
void 
kam_dui_mkdir_()
{
   char*  path = ku_gets();	/* path */

   dui_mkdir(path);
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
void 
kam_dui_mv_()
{
   char*  fromPath = ku_gets();	/* fromPath */
   char*  toPath = ku_gets();	/* toPath */

   dui_mv(fromPath, toPath);
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
void 
kam_dui_pwd_()
{
  dui_pwd();
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
void 
kam_dui_rm_()
{
   char*  path = ku_gets();	/* path */

   dui_rm(path);
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
void 
kam_dui_rmdir_()
{
   char*  path = ku_gets();	/* path */

   dui_rmdir(path);
}

void kam_dui_precious_() {
  dui_precious();
}
void kam_dui_rm_nonprecious_() {
  dui_rm_nonprecious();
}
