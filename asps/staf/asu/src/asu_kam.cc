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

