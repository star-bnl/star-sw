/*:Copyright 1996, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         tbr_kam.c
*:DESCRIPTION:  C KUIP Action Modules for TBR
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      11mar96-v000a-cet,hjw- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "kuip.h"
#include "tbr_types.h"

#include "emlLib.h"
#include "duiLib.h"

#include "tbrLib.h"

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tbr_count_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TBR/COUNT
*:<---------------------------------------------------------------------
*/
void kam_tbr_count_(){kam_tbr_count();}
int kam_tbr_count()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   printf("TBR:\tObject count = %d \n",tbr->count());
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tbr_list_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TBR/COUNT
*:<---------------------------------------------------------------------
*/
void kam_tbr_list_(){kam_tbr_list();}
int kam_tbr_list()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   printf("%s",tbr->list() );
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tbr_motif_viewdataset_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TBR/LAUNCH
*:<---------------------------------------------------------------------
*/
void kam_tbr_motif_viewdataset_(){kam_tbr_motif_viewdataset();}
int kam_tbr_motif_viewdataset()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   if( !tbr_MotifViewer->viewDataset(dui->rootDO()) ){ /* Craig. Used to be dui
                                                       ** intead of tdm */
      EML_ERROR(KAM_METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

