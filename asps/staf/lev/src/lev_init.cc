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
   printf("LEV: Initializing.\n");

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
  printf("LEV: Starting.\n"); fflush(0);
  lev = new levFactory("lev");
  printf("LEV: Updating.\n"); fflush(0);
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
   printf("LEV: Stopping.\n");

   return TRUE;
}

