//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	spx_init.C
//:DESCRIPTION:	Functions  to initialize SPX 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	21nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include "asuLib.h"
#include "emlLib.h"
#include "spxLib.h"

#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define spx_def_ F77_NAME(spx_def,SPX_DEF)
extern "C" void type_of_call spx_def_();

spxFactory *spx;

//:>--------------------------------------------------------------------
//:ROUTINE:	int spx_init()
//:DESCRIPTION:	Initialize SPX
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int spx_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("SPX:Initializing. ");
#endif

/*- Define the SPX KUIP commands. -*/
   spx_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int spx_start()
//:DESCRIPTION:	Start SPX
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int spx_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("SPX:Starting. ");
#endif

/*- Create the SPX Factory. -*/
   spx = new spxFactory("spx");
   
   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int spx_stop()
//:DESCRIPTION:	Stop SPX
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int spx_stop()
{
#ifndef QUIET_ASP
   EML_MESSAGE("SPX:Stopping. ");
#endif

/*- Delete the SPX Factory.
   delete spx;
unecessary -- soc will do it. -*/

   return TRUE;
}

