//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	tbr_init.C
//:DESCRIPTION:	Functions  to initialize TBR 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	08dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

//:>---------------------------------------------- INCLUDES ------------
#include "tbr_macros.h"
#include "tbr_types.h"
#include "tbrClasses.hh"
#include "tbr_globals.h"

#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define tbr_def_ F77_NAME(tbr_def,TBR_DEF)
extern "C" void type_of_call tbr_def_();

#ifndef WIN32
#include <X11/Intrinsic.h>

tbrFactory *tbr;
tbrMotifViewer *tbr_MotifViewer;	/* HACK */

//:>--------------------------------------------------------------------
//:ROUTINE:	int tbr_init()
//:DESCRIPTION:	Initialize TBR
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tbr_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TBR:Initializing. ");
#endif

/*- Define the TBR KUIP commands. -*/
   tbr_def_();

/*- Initializing X Toolkit. -*/
   XtToolkitInitialize();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int tbr_start()
//:DESCRIPTION:	Start TBR
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tbr_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TBR:Starting. ");
#endif

/*- Create the TBR Viewers. -*/
   tbr = new tbrFactory("tbr");
   tbr_MotifViewer = new tbrMotifViewer("tbr_MotifViewer"); /* HACK */

   return TRUE;
}
#else
/*  Define dummy for WIN32 */
int tbr_init()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TBR:Initializing.  Dummy ! ! ! ");
#endif
   return FALSE;
}

int tbr_start()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TBR:Starting. It is dummy under WIN32");
#endif
   return FALSE;
}

#endif /* WIN32 */ 
//:>--------------------------------------------------------------------
//:ROUTINE:	int tbr_stop()
//:DESCRIPTION:	Stop TBR
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tbr_stop()
{
#ifndef QUIET_ASP
   EML_MESSAGE("TBR:Stopping. ");
#endif

   return TRUE;
}
