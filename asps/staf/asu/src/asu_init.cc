//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	asu_init.C
//:DESCRIPTION:	Functions  to initialize ASU 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	20nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <string.h>
#include <stdio.h>
#include "asuAlloc.h"
#include "asuLib.h"	/* Analysis Service Utilities */
#include "emlLib.h"	/* Error Messaging & Logging */


//:>--------------------------------------------------------------------
//:ROUTINE:	int asu_init()
//:DESCRIPTION:	Initialize ASU
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int asu_init()
{
   EML_MESSAGE("Initializing ASU.");

/*- Setup basic KUIP commands. -*/
   asu_kuip_init_();

/*- Define the ASU KUIP commands. -*/
   asu_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int asu_start()
//:DESCRIPTION:	Start ASU
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int asu_start()
{
   EML_MESSAGE("Starting ASU.");

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int asu_stop()
//:DESCRIPTION:	Stop ASU
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int asu_stop()
{
   EML_MESSAGE("Stopping ASU.");

   asuAllocStats();			/* show allocation stats */
   return TRUE;
}

