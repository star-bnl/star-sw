//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	asu_init.C
//:DESCRIPTION:	Functions  to initialize ASU 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	21jun96-v001a-cet- remove asu_kuip_init_
//:HISTORY:	20nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <string.h>
#include <stdio.h>
#include "asuAlloc.h"
#include "asuLib.h"	/* Analysis Service Utilities */
#include "emlLib.h"	/* Error Messaging & Logging */

extern "C" void staf_kuip_init_();

//:>--------------------------------------------------------------------
//:ROUTINE:	int asu_init()
//:DESCRIPTION:	Initialize ASU
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int asu_init()
{
   EML_MESSAGE("ASU:Initializing. ");

   asuMallocInit();			/* initalize asuAlloc */

/*- Setup basic KUIP commands. -*/
/* asu_kuip_init_();------------- use STAF_CERN_INIT in main instead */
/*22jul97 this is called in main()   staf_kuip_init_(); */

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
   EML_MESSAGE("ASU:Starting. ");

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
   EML_MESSAGE("ASU:Stopping. ");

   asuMallocStats();			/* show allocation stats */
   return TRUE;
}

