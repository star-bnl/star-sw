//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	dio_init.C
//:DESCRIPTION:	Functions  to initialize DIO 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	08dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include "asuLib.h"
#include "emlLib.h"
#include "dio_macros.h"
#include "dio_types.h"
#include "dioClasses.hh"
#include "dio_globals.h"

#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define dio_def_ F77_NAME(dio_def,DIO_DEF)
extern "C" void type_of_call dio_def_();

dioFactory *dio;

//:>--------------------------------------------------------------------
//:ROUTINE:	int dio_init()
//:DESCRIPTION:	Initialize DIO
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int dio_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("DIO:Initializing. ");
#endif

/*- Load other PKGs. -*/
// soc->bind("tdm");

//						ifdef KUIP
/*- Define the DIO KUIP commands. -*/
   dio_def_();
//						endif /* KUIP */

#ifdef TCL
/*- Define the DIO TCL commands. -*/
   dio_def_();
#endif /* TCL */

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int dio_start()
//:DESCRIPTION:	Start DIO
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int dio_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("DIO:Starting. ");
#endif

/*- Create the DIO Factory. -*/
   dio = new dioFactory("dio");

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int dio_stop()
//:DESCRIPTION:	Stop DIO
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int dio_stop()
{
#ifndef QUIET_ASP
   EML_MESSAGE("DIO:Stopping. ");
#endif

/*- Delete the DIO Factory.
   delete dio;
unecessary -- soc will do it. -*/

   return TRUE;
}

