//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	soc_init.C
//:DESCRIPTION:	Functions  to initialize SOC 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	14nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <stdio.h>
#include "asuLib.h"
#include "emlLib.h"
#include "soc_macros.h"
#include "soc_types.h"
#include "soc_globals.h"

socCatalog *soc;

//:>--------------------------------------------------------------------
//:ROUTINE:	int soc_init()
//:DESCRIPTION:	Initialize SOC
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int soc_init()
{
   EML_MESSAGE(Initializing SOC.);

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
   EML_MESSAGE(Starting SOC.);

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
   EML_MESSAGE(Stopping SOC.);

/*- Delete the SOC Catalog. -*/
   delete soc;

   return TRUE;
}

