/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         lev_kam.c
*:DESCRIPTION:  C KUIP Action Modules for LEV
*:AUTHOR:       hjw - Herb Ward - ward@physics.utexas.edu
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      01jul96-v000a-hjw- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "kuip.h"

#include "dui_globals.h"
#include "lev_macros.h"
#include "lev_types.h"
#include "lev_globals.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_lev_log_env_info_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* LEV/LAUNCH
*:<---------------------------------------------------------------------
*/
void kam_lev_update_()
{
  //Later maybe. long npars = ku_npar();      /* number of KUIP parameters */
  lev->update();
  printf("I have updated the versions table (config/levVersions).\n");
  set_staf_status(0);
}
void lev_registerversion_(const char *name,const char *version) {
  lev->registerVersion(name,"pam",version);
}
void lev_registerversion(const char *name,const char *version) {
  lev_registerversion_(name,version);
}
