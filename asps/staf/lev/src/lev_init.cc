//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	lev_init.C
//:DESCRIPTION:	Functions  to initialize LEV 
//:AUTHOR:	hjw - Herb Ward - ward@physics.utexas.edu
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	01jul96-v000a-hjw- creation
//:<--------------------------------------------------------------------

//:>---------------------------------------------- INCLUDES ------------
#include "lev_macros.h"
#include "lev_types.h"
#include "levClasses.hh"
#include "lev_globals.h"

#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define lev_def_ F77_NAME(lev_def,LEV_DEF)
extern "C" void type_of_call lev_def_();

//Later.  levFactory *lev_Factory;
levFactory *lev;

//:>--------------------------------------------------------------------
//:ROUTINE:	int lev_init()
//:DESCRIPTION:	Initialize LEV
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int lev_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("LEV:Initializing. ");
#endif

/*- Define the LEV KUIP commands. -*/
   lev_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int lev_start()
//:DESCRIPTION:	Start LEV
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int lev_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("LEV:Starting. "); fflush(0);
#endif
   lev = new levFactory("lev");
#ifndef QUIET_ASP
   EML_MESSAGE("LEV:Updating. "); fflush(0);
#endif
   lev->levUpdate();
   // What's this for?   tdmTable *versions=lev->versions();
   // What's this for?   tdmTable *environment=lev->environment();
   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int lev_stop()
//:DESCRIPTION:	Stop LEV
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int lev_stop()
{
#ifndef QUIET_ASP
   EML_MESSAGE("LEV:Stopping. ");
#endif

   return TRUE;
}

