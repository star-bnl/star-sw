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
#define EML_MAIN TRUE
#include "asuLib.h"	/* Analysis Service Utilities */
#include "emlLib.h"	/* Error Messaging and Logging */

#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define   eml_kuvec_init_ F77_NAME(eml_kuvec_init,EML_KUVEC_INIT)
// #define eml_def_ F77_NAME(eml_def,EML_DEF)

extern CC_P void type_of_call eml_kuvec_init_();
// extern CC_P void type_of_call eml_def_();

//:>--------------------------------------------------------------------
//:ROUTINE:	int eml_init()
//:DESCRIPTION:	Initialize EML
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int eml_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("EML:Initializing. ");
#endif

/*- Define the EML KUIP commands. -*/
//   eml_def_();

/*- Initialize STAF vectors. -*/
/* 23OCT96 ---- TEMPORARY DEBUG HACK ----
			15may97 - REMOVE HACK... vectors were missing
-- 22JUL97 ---- TEMPORARY DEBUG REHACK --*/
   eml_kuvec_init_();
   EML_INIT();
   eml_pretty_on=7;          /* make error messages pretty */
   eml_beep_on=7;            /* beep terminal on error */
   eml_demand_ack_on=0;      /* beep terminal on error */

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
#ifndef QUIET_ASP
   EML_MESSAGE("EML:Starting. ");
#endif

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
#ifndef QUIET_ASP
   EML_MESSAGE("EML:Stopping. ");
#endif

   return TRUE;
}

