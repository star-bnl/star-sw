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

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_tbr_count_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* TBR/COUNT
*:<---------------------------------------------------------------------
*/
void 
kam_tbr_count_()
{
  tbr_count();
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
void 
kam_tbr_list_()
{
  tbr_list();
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
void 
kam_tbr_viewdataset_()
{
  tbr_viewdataset();
}

