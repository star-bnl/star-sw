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
#include "fortranc.h"   /* The definition of the Fortran/C interface */

#define tnt_def_ F77_NAME(tnt_def,TNT_DEF)
extern "C" void type_of_call tnt_def_();

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
#ifndef QUIET_ASP
   EML_MESSAGE("TNT:Initializing. ");
#endif

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
#ifndef QUIET_ASP
   EML_MESSAGE("TNT:Starting. ");
#endif

/*- Create the TNT Factory. -*/
   tnt = new tntFactory("tnt");

#ifndef QUIET_ASP
   EML_MESSAGE("TNT:Started. ");
#endif
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
#ifndef QUIET_ASP
   EML_MESSAGE("TNT:Stopping. ");
#endif

/*- Delete the TNT Factory.
   tnt->lock(FALSE);
   delete tnt;
unecessary -- soc will do it. -*/

   return TRUE;
}

