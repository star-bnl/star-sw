//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	dui_init.C
//:DESCRIPTION:	Functions  to initialize DUI 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	08dec95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include "dstype.h"
#include "dui_macros.h"
#include "dui_types.h"
#include "duiClasses.hh"
#include "dui_globals.h"

   duiDispatcher *dui;
//- This is a global that may not be preserved. --
   DS_DATASET_T *dui_pDScwd;

//- SUPERCEED TDM_FACTORY --
#include "soc_globals.h"
#include "tdm_globals.h"

//:>--------------------------------------------------------------------
//:ROUTINE:	int dui_init()
//:DESCRIPTION:	Initialize DUI
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int dui_init()
{
   EML_MESSAGE(Initializing DUI.);

/*- Define the DUI KUIP commands. -*/
   dui_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int dui_start()
//:DESCRIPTION:	Start DUI
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int dui_start()
{
   EML_MESSAGE(Starting DUI.);

/*- Create the DUI Dispatcher. -*/
   dui = new duiDispatcher("dui");

/*- Superceed the TDM Factory. -*/
   if( tdm != NULL ){
      EML_MESSAGE(DUI superceeds TDM.);
      tdm->lock(FALSE);
      soc->deleteObject("tdm","tdmFactory");
      tdm = dui;
   }

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int dui_stop()
//:DESCRIPTION:	Stop DUI
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int dui_stop()
{
   EML_MESSAGE(Stopping DUI.);

/*- Delete the DUI Dispatcher.
   delete dui;
unecessary -- soc will do it. -*/

   return TRUE;
}

