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

#include <X11/Intrinsic.h>

tbrFactory *tbr;

//:>--------------------------------------------------------------------
//:ROUTINE:	int tbr_init()
//:DESCRIPTION:	Initialize TBR
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tbr_init()
{
   EML_MESSAGE("Initializing TBR.");

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
   EML_MESSAGE("Starting TBR.");

/*- Create the TBR Viewers. -*/
   tbr = new tbrFactory("tbr");
   tbr_MotifViewer = new tbrMotifViewer("tbr_MotifViewer"); /* HACK */

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int tbr_stop()
//:DESCRIPTION:	Stop TBR
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tbr_stop()
{
   EML_MESSAGE("Stopping TBR.");

   return TRUE;
}

