//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	soc_init.C
//:DESCRIPTION:	Functions  to initialize SOC 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	14nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <stdio.h>
#define KUIP
#include "asuLib.h"
#include "emlLib.h"
#include "soc_macros.h"
#include "soc_types.h"
#include "soc_globals.h"

#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define soc_def_ F77_NAME(soc_def,SOC_DEF)
extern "C" void type_of_call soc_def_();

socCatalog *soc;

//:>--------------------------------------------------------------------
//:ROUTINE:	int soc_init()
//:DESCRIPTION:	Initialize SOC
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int soc_init()
{
  // EML_MESSAGE("SOC:Initializing. ");

/*- Define the SOC KUIP commands. -*/
   soc_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int soc_start()
//:DESCRIPTION:	Start SOC
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int soc_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("SOC:Starting. ");
#endif

/*- Create the SOC Catalog. -*/
   soc = new socCatalog();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int soc_stop()
//:DESCRIPTION:	Stop SOC
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int soc_stop()
{
#ifndef QUIET_ASP
   EML_MESSAGE("SOC:Stopping. ");
#endif

/*- Delete the SOC Catalog. -*/
   delete soc;

   return TRUE;
}

