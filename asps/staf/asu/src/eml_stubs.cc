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

#include "fortranc.h"

#define set_staf_status_ F77_NAME(set_staf_status,SET_STAF_STATUS)
#define get_staf_status_ F77_NAME(get_staf_status,GET_STAF_STATUS)
#define set_staf_result_ F77_NAME(set_staf_result,SET_STAF_RESULT)
#define get_staf_result_ F77_NAME(get_staf_result,GET_STAF_RESULT)

extern "C" { 
            void  type_of_call set_staf_status_(long *);
            long  type_of_call get_staf_status_(long *);

            void  type_of_call set_staf_result_(float *);
            float type_of_call get_staf_result_(long *);
           }

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
