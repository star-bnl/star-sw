/*:Copyright 1995, Lawrence Berkeley National Laboratory
*:>---------------------------------------------------------------------
*:FILE:         asu_kam.c
*:DESCRIPTION:  C KUIP Action Modules for ASU
*:AUTHOR:       cet - Craig E. Tull, cetull@lbl.gov
*:BUGS:         -- STILL IN DEVELOPMENT --
*:HISTORY:      12feb96-v000a-cet- creation
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#if defined(irix) || defined(irix64) || defined(Linux)
# include <sys/time.h>
#else
# include <time.h>
#endif
#ifndef WIN32
# include <unistd.h>
#else
# include <winsock.h>
#endif /* WIN32 */

#include "kuip.h"
#define KUIP

#include "asuAlloc.h"
#include "asuLib.h"	/* Analysis Service Utilities */
#include "emlLib.h"	/* Error Messaging & Logging */

#ifdef WIN32
  int gettimeofday(struct timeval *tp, void *ptr){return 0;}
#endif /* WIN32 */ 

/*-------------------------------------------- TYPEDEFS             --*/
/*-------------------------------------------- GLOBALS              --*/
/*-------------------------------------------- PROTOTYPES           --*/

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asu_hello_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/HELLO [ MESSAGE ]
*:<---------------------------------------------------------------------
*/
void 
kam_asu_hello_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  msg = ku_gets();	/* message */

	STAFCV_T status = asu_hello(msg);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asu_time_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/TIME
*:<---------------------------------------------------------------------
*/
void 
kam_asu_demand_ack_() {
  char *value = ku_gets();
  asu_demand_ack(value);
}

void 
kam_asu_beep_() {
  char *value = ku_gets();
  asu_beep(value);
}
 
void 
kam_asu_pretty_() {
  char *value = ku_gets();
  asu_pretty(value);
}

void 
kam_asu_time_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   STAFCV_T status = asu_time();
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asu_date_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/TIME
*:<---------------------------------------------------------------------
*/
void 
kam_asu_date_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   STAFCV_T status = asu_date();
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asu_fflush_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/TIME
*:<---------------------------------------------------------------------
*/
void 
kam_asu_fflush_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

   STAFCV_T status = asu_fflush();
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asumalloc_stats_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/TIME
*:<---------------------------------------------------------------------
*/
void 
kam_asumalloc_stats_()
{
  STAFCV_T status = asumalloc_stats();
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asumalloc_level_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/TIME
*:<---------------------------------------------------------------------
*/
void
kam_asumalloc_level_()
{
  int level = ku_geti();
  STAFCV_T status = asumalloc_level(level);
}
