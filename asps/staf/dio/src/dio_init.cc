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

dioFactory *dio;

//:>--------------------------------------------------------------------
//:ROUTINE:	int dio_init()
//:DESCRIPTION:	Initialize DIO
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int dio_init()
{
   EML_MESSAGE(DIO: Initializing.);

/*- Define the DIO KUIP commands. -*/
   dio_def_();

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
   EML_MESSAGE(DIO: Starting.);

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
   EML_MESSAGE(DIO: Stopping.);

/*- Delete the DIO Factory.
   delete dio;
unecessary -- soc will do it. -*/

   return TRUE;
}

