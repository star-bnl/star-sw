//:Copyright 1995, Lawrence Berkeley National Laboratory
//:>--------------------------------------------------------------------
//:FILE:	tdm_init.C
//:DESCRIPTION:	Functions  to initialize TDM 
//:AUTHOR:	cet - Craig E. Tull, cetull@lbl.gov
//:BUGS:	-- STILL IN DEVELOPMENT --
//:HISTORY:	29nov95-v000a-cet- creation
//:<--------------------------------------------------------------------

#include "tdmLib.h"

tdmFactory *tdm;

//:>--------------------------------------------------------------------
//:ROUTINE:	int tdm_init()
//:DESCRIPTION:	Initialize TDM
//:ARGUMENTS:	-- NONE --
//:RETURN VALUE:-- NONE --
//:<--------------------------------------------------------------------
int tdm_init()
{
   EML_MESSAGE(Initializing TDM.);

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
   EML_MESSAGE(Starting TDM.);

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
   EML_MESSAGE(Stopping TDM.);

/*- Delete the TDM Factory.
   delete tdm;
unecessary -- soc will do it. -*/

   dsDatasetAllocStats();		/* show allocation stats */
   return TRUE;
}

