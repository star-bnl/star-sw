//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	eml_init.C
//:DESCRIPTION:	Functions  to initialize EML 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	20nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include <string.h>
#include <stdio.h>
#include "asuLib.h"	/* Analysis Service Utilities */
#define EML_MAIN
#include "emlLib.h"	/* Error Messaging and Logging */


//:>--------------------------------------------------------------------
//:ROUTINE:	int eml_init()
//:DESCRIPTION:	Initialize EML
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int eml_init()
{
   EML_MESSAGE("EML: Initializing.");

/*- Define the EML KUIP commands. -*/
// eml_def_();

/*- Initialize STAF vectors. -*/
/* 23OCT96 ---- TEMPORARY DEBUG HACK ----
   eml_kuvec_init_();
-- 23OCT96 ---- TEMPORARY DEBUG HACK --*/

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int eml_start()
//:DESCRIPTION:	Start EML
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int eml_start()
{
   EML_MESSAGE("EML: Starting.");

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int eml_stop()
//:DESCRIPTION:	Stop EML
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int eml_stop()
{
   EML_MESSAGE("EML: Stopping.");

   return TRUE;
}

