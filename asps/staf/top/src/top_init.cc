//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	top_init.C
//:DESCRIPTION:	Functions  to initialize TOP 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	29nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include "topLib.h"
#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define top_def_ F77_NAME(top_def,TOP_DEF)
extern "C" void type_of_call top_def_();

topFactory *top;

//:>--------------------------------------------------------------------
//:ROUTINE:	int top_init()
//:DESCRIPTION:	Initialize TOP
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int top_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TOP:Initializing. ");
#endif

/*- Define the TOP KUIP commands. -*/
   top_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int top_start()
//:DESCRIPTION:	Start TOP
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int top_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TOP:Starting. ");
#endif

/*- Create the TOP Factory. -*/
   top = new topFactory("top");

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int top_stop()
//:DESCRIPTION:	Stop TOP
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int top_stop()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TOP:Stopping. ");
#endif

/*- Delete the TOP Factory.
   delete top;
unecessary -- soc will do it. -*/

   return TRUE;
}

