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

   duiFactory *dui;
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
   EML_MESSAGE("DUI:Initializing. ");

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
   EML_MESSAGE("DUI:Starting. ");

/*- Create the DUI Dispatcher. -*/
   dui = new duiFactory("dui");

/*- Find the TDM Factory. -*/
   socObject *obj=NULL;
   if( NULL == (obj = soc->findObject("tdm","tdmFactory")) ){
      return TRUE;
   }
   tdmFactory *t = (tdmFactory*)(obj->ptr());
/*- Superceed the TDM Factory. -*/
   if( t != NULL ){
      EML_WARNING("DUI:Superceeding TDM. ");
      t->lock(FALSE);
      soc->deleteObject("tdm","tdmFactory");
      t = dui;
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
   EML_MESSAGE("DUI:Stopping. ");

/*- Delete the DUI Dispatcher.
   delete dui;
unecessary -- soc will do it. -*/

   return TRUE;
}

