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
#include <time.h>

#include "kuip.h"
#define KUIP

#include "asuAlloc.h"
#include "asuLib.h"	/* Analysis Service Utilities */
#include "emlLib.h"	/* Error Messaging & Logging */

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
void kam_asu_hello_()
{
   long npars = ku_npar();      /* number of KUIP parameters */
   char*  msg = ku_gets();	/* message */

	STAFCV_T status = asu_hello(msg);
}
STAFCV_T asu_hello(char* msg)
{
   printf("ASU:\tHello, %s \n",msg);
   EML_SUCCESS(STAFCV_OK);
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
void kam_asu_time_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

	STAFCV_T status = asu_time();
}
STAFCV_T asu_time()
{
   time_t it,*pt=&it;

   *pt = time(0);
   printf("ASU:\tTime = %s \n",ctime(pt));
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asuallocstats_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/TIME
*:<---------------------------------------------------------------------
*/
void kam_asuallocstats_()
{
	STAFCV_T status = asuallocstats();
}
STAFCV_T asuallocstats()
{
   asuMallocStats();
   EML_SUCCESS(STAFCV_OK);
}

/*
*:>---------------------------------------------------------------------
*:ROUTINE:      void kam_asualloclevel_
*:DESCRIPTION:  KUIP Action Module to ...
*:ARGUMENTS:    -- NONE --
*:RETURN VALUE: -- NONE --
*:* ASU/TIME
*:<---------------------------------------------------------------------
*/
void kam_asualloclevel_()
{
   int level = ku_geti();
	STAFCV_T status = asualloclevel(level);
}
STAFCV_T asualloclevel(int level)
{
   ASU_MALLOCLEVEL_T mLevel=ASU_MALLOC_FAST;

   switch(level) {
   case 0:
      printf("ASU/MALLOC/LEVEL = ");fflush(0);
      mLevel=asuMallocLevel(ASU_MALLOC_FAST);
      switch(mLevel) {
      case ASU_MALLOC_INIT:     /* UNINITIALIZED */
         printf("ASU_MALLOC_INIT\n");fflush(0);
	 break;
      case ASU_MALLOC_FAST:     /* only call m&f */
         printf("ASU_MALLOC_FAST\n");fflush(0);
	 break;
      case ASU_MALLOC_COUNT:    /* ...and count calls to m&f */
         printf("ASU_MALLOC_COUNT\n");fflush(0);
	 break;
      case ASU_MALLOC_TRACE:    /* ...and keep trace of m&f */
         printf("ASU_MALLOC_TRACE\n");fflush(0);
	 break;
      case ASU_MALLOC_FILL:     /* ...and fill w/ pattern */
         printf("ASU_MALLOC_FILL\n");fflush(0);
	 break;
      case ASU_MALLOC_VERBOSE:  /* ...and print every time */
         printf("ASU_MALLOC_VERBOSE\n");fflush(0);
	 break;
      default:
         printf("UNKNOWN\n");fflush(0);
	 break;
      }
      break;
   case 1:
      mLevel=ASU_MALLOC_FAST;
      break;
   case 2:
      mLevel=ASU_MALLOC_COUNT;
      break;
   case 3:
      mLevel=ASU_MALLOC_TRACE;
      break;
   case 4:
      mLevel=ASU_MALLOC_FILL;
      break;
   case 5:
      mLevel=ASU_MALLOC_VERBOSE;
      break;
   default:
      break;
   }
   asuMallocLevel(mLevel);
   EML_SUCCESS(STAFCV_OK);
}

