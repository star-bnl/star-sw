/*:Copyright 1996, Lawrence Berkeley National Laboratory
**:>--------------------------------------------------------------------
**:FILE:        top_init.C
**:DESCRIPTION: Functions  to initialize TOP-Table OPerators
**:AUTHOR:      cet - Craig E. Tull, cetull@lbl.gov
**:BUGS:        -- STILL IN DEVELOPMENT --
**:HISTORY:     24oct96-v000a-cet- creation
**:<--------------------------------------------------------------------
*/

#include "topLib.h"

topFactory *top;

/*
**:>--------------------------------------------------------------------
**:ROUTINE:     int top_init()
**:DESCRIPTION: Initialize top
**:ARGUMENTS:   -- NONE --
**:RETURN VALUE:-- NONE --
**:<--------------------------------------------------------------------
*/
int top_init()
{
   EML_MESSAGE(TOP: Initializing.);

/*- Define the TOP KUIP commands. -*/
   top_def_();

   return TRUE;
}

/*
**:>--------------------------------------------------------------------
**:ROUTINE:     int top_start()
**:DESCRIPTION: Start TOP
**:ARGUMENTS:   -- NONE --
**:RETURN VALUE:-- NONE --
**:<--------------------------------------------------------------------
*/
int top_start()
{
   EML_MESSAGE(TOP: Starting.);

/*- Create the TOP Factory. -*/
   top = new topFactory("top");

   return TRUE;
}

/*
**:>--------------------------------------------------------------------
**:ROUTINE:     int top_stop()
**:DESCRIPTION: Stop TOP
**:ARGUMENTS:   -- NONE --
**:RETURN VALUE:-- NONE --
**:<--------------------------------------------------------------------
*/
int top_stop()
{
   EML_MESSAGE(TOP: Stopping.);

/*- Delete the TOP Factory.
   top->lock(FALSE);
   delete top;
unecessary -- soc will do it. -*/

   return TRUE;
}

