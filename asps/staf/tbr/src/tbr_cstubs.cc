/*:
*:>---------------------------------------------------------------------
*:FILE:         tbr_kam.c
*:DESCRIPTION:  C KUIP Action Modules for TBR
*:AUTHOR:       
*:BUGS:         
*:HISTORY:      
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>

#include "emlLib.h"
#include "duiLib.h"

#include "tbrLib.h"

STAFCV_T 
tbr_count()
{
  printf("TBR:\tObject count = %ld \n",tbr->count());
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tbr_list()
{
  printf("%s",tbr->list() );
  EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
tbr_viewdataset()
{
  if( !tbr_MotifViewer->viewDataset(dui->rootDO()) ){
    EML_CONTEXT("ERROR: This is not your fault.\n");
    EML_FAILURE(METHOD_FAILURE);
  }
  EML_SUCCESS(STAFCV_OK);
}

