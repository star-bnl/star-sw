/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        tnt_init.C
**:DESCRIPTION: Functions  to initialize TNT-TEMPLATE ASP
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     13jun96-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#include "tntLib.h"
#include "tntHBOOK.h"

tntFactory *tnt;

/*
**:>--------------------------------------------------------------------
**:ROUTINE:     int tnt_init()
**:DESCRIPTION: Initialize TNT
**:ARGUMENTS:   -- NONE --
**:RETURN VALUE:-- NONE --
**:<--------------------------------------------------------------------
*/
int tnt_init()
{
   EML_MESSAGE("TNT:Initializing. ");

/*- Define the TNT KUIP commands. -*/
   tnt_def_();

/*- Initialize the PAWC array. -*/
// initpawc_();

   return TRUE;
}

/*
**:>--------------------------------------------------------------------
**:ROUTINE:     int tnt_start()
**:DESCRIPTION: Start TNT
**:ARGUMENTS:   -- NONE --
**:RETURN VALUE:-- NONE --
**:<--------------------------------------------------------------------
*/
int tnt_start()
{
   EML_MESSAGE("TNT:Starting. ");

/*- Create the TNT Factory. -*/
   tnt = new tntFactory("tnt");

   EML_MESSAGE("TNT:Started. ");
   return TRUE;
}

/*
**:>--------------------------------------------------------------------
**:ROUTINE:     int tnt_stop()
**:DESCRIPTION: Stop TNT
**:ARGUMENTS:   -- NONE --
**:RETURN VALUE:-- NONE --
**:<--------------------------------------------------------------------
*/
int tnt_stop()
{
   EML_MESSAGE("TNT:Stopping. ");

/*- Delete the TNT Factory.
   tnt->lock(FALSE);
   delete tnt;
unecessary -- soc will do it. -*/

   return TRUE;
}

