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
void kam_tbr_count_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = tbr_count();
}
STAFCV_T tbr_count()
{
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
void kam_tbr_list_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = tbr_list();
}
STAFCV_T tbr_list()
{
   printf("%s",tbr->list() );
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tbr_viewdataset_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TBR/LAUNCH
*:<---------------------------------------------------------------------
*/
void kam_tbr_viewdataset_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = tbr_viewdataset();
}
STAFCV_T tbr_viewdataset()
{
   if( !tbr_MotifViewer->viewDataset(dui->rootDO()) ){
      EML_CONTEXT("ERROR: This is not your fault.\n");
      EML_FAILURE(METHOD_FAILURE);
   }
   EML_SUCCESS(STAFCV_OK);
}

