//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	asu_init.C
//:DESCRIPTION:	Functions  to initialize ASU 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	21jun96-v001a-cet- remove asu_kuip_init_
//:HISTORY:	20nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#include "asuAlloc.h"
#include "asuLib.h"	/* Analysis Service Utilities */
#include "emlLib.h"	/* Error Messaging & Logging */

#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define asu_def_ F77_NAME(asu_def,ASU_DEF)
extern "C" void type_of_call asu_def_();

//:>--------------------------------------------------------------------
//:ROUTINE:	int asu_init()
//:DESCRIPTION:	Initialize ASU
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int asu_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("ASU:Initializing. ");
#endif

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
#ifndef QUIET_ASP
   EML_MESSAGE("ASU:Starting. ");
#endif

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
#ifndef QUIET_ASP
   EML_MESSAGE("ASU:Stopping. ");
#endif

   asuMallocStats(-1);			/* show allocation stats */
   return TRUE;
}

