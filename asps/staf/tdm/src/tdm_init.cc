//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	tdm_init.C
//:DESCRIPTION:	Functions  to initialize TDM 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	29nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include "tdmLib.h"

extern "C" int tdm_init();
extern "C" int tdm_start();
extern "C" int tdm_stop();

tdmFactory *tdm;

//:>--------------------------------------------------------------------
//:ROUTINE:	int tdm_init()
//:DESCRIPTION:	Initialize TDM
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tdm_init()
{
   EML_MESSAGE("TDM:Initializing. ");

/*- Define the TDM KUIP commands. -*/
   tdm_def_();

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int tdm_start()
//:DESCRIPTION:	Start TDM
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tdm_start()
{
   EML_MESSAGE("TDM:Starting. ");

/*- Create the TDM Factory. -*/
   tdm = new tdmFactory("tdm");

   return TRUE;
}

//:>--------------------------------------------------------------------
//:ROUTINE:	int tdm_stop()
//:DESCRIPTION:	Stop TDM
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tdm_stop()
{
   EML_MESSAGE("TDM:Stopping. ");

/*- Delete the TDM Factory.
   delete tdm;
unecessary -- soc will do it. -*/

   printf("\n");
#ifndef OLD_DSL
   dsAllocStats();			/* show allocation stats */
#else   /*OLD_DSL*/
   dsDatasetAllocStats();		/* show allocation stats */
#endif  /*OLD_DSL*/

   return TRUE;
}

