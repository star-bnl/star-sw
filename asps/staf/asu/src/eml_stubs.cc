/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         asu_stubs.c
*:DESCRIPTION:  C Stubs for ASU.
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      18mar96-v000b-cet- rework
*:HISTORY:      01dec95-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#include "asu_macros.h"
#include "asu_types.h"

void set_staf_status(long status)
{
   set_staf_status_(&status);
}

long get_staf_status(long n)
{
   return get_staf_status_(&n);
}

void set_staf_result(float result)
{
   set_staf_result_(&result);
}

float get_staf_result(long n)
{
   return get_staf_result_(&n);
}
