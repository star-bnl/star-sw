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
#ifdef irix
#include <sys/time.h>
#else
#include <time.h>
#endif
#include <unistd.h>

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
void kam_asu_demand_ack_() {
  char *value = ku_gets();
  asu_demand_ack(value);
}
STAFCV_T asu_demand_ack(char *value) {
  int ii; char val[200]; strncpy(val,value,195); val[195]=0;
  for(ii=0;val[ii];ii++) { if(val[ii]>='a'&&val[ii]<='z') val[ii]+='A'-'a'; }
  if(!strcmp(val,"SHOW")) {
    if(eml_demand_ack_on) printf("ASU: demand error acknowledgement is ON.\n");
    else                  printf("ASU: demand error acknowledgement is OFF.\n");
    EML_SUCCESS(STAFCV_OK);
  }
  if(!strcmp(val,"FALSE")||!strcmp(val,"OFF")) {
    eml_demand_ack_on=0; // I use 0 for FALSE.
    EML_SUCCESS(STAFCV_OK);
  }
  if(!strcmp(val,"TRUE")||!strcmp(val,"ON")) {
    eml_demand_ack_on=7; // I use 7 for TRUE.
    EML_SUCCESS(STAFCV_OK);
  }
  EML_CONTEXT("ERROR: I want 'true', 'false', 'show', or nothing.\n");
  EML_FAILURE(INVALID_COMMAND_PARAMETER);
}
void kam_asu_beep_() {
  char *value = ku_gets();
  asu_beep(value);
}
STAFCV_T asu_beep(char *value) {
  int ii; char val[200]; strncpy(val,value,195); val[195]=0;
  for(ii=0;val[ii];ii++) { if(val[ii]>='a'&&val[ii]<='z') val[ii]+='A'-'a'; }
  if(!strcmp(val,"SHOW")) {
    if(eml_beep_on) printf("ASU: beep on error is ON.\n");
    else            printf("ASU: beep on error is OFF.\n");
    EML_SUCCESS(STAFCV_OK);
  }
  if(!strcmp(val,"FALSE")||!strcmp(val,"OFF")) {
    eml_beep_on=0; // I use 0 for FALSE.
    EML_SUCCESS(STAFCV_OK);
  }
  if(!strcmp(val,"TRUE")||!strcmp(val,"ON")) {
    eml_beep_on=7; // I use 7 for TRUE.
    EML_SUCCESS(STAFCV_OK);
  }
  EML_CONTEXT("ERROR: I want 'true', 'false', 'show', or nothing.\n");
  EML_FAILURE(INVALID_COMMAND_PARAMETER);
}
void kam_asu_pretty_() {
  char *value = ku_gets();
  asu_pretty(value);
}
STAFCV_T asu_pretty(char *value) {
  int ii; char val[200]; strncpy(val,value,195); val[195]=0;
  for(ii=0;val[ii];ii++) { if(val[ii]>='a'&&val[ii]<='z') val[ii]+='A'-'a'; }
  if(!strcmp(val,"SHOW")) {
    if(eml_pretty_on) printf("ASU: pretty error messaging is ON.\n");
    else              printf("ASU: pretty error messaging is OFF.\n");
    EML_SUCCESS(STAFCV_OK);
  }
  if(!strcmp(val,"FALSE")||!strcmp(val,"OFF")) {
    eml_pretty_on=0; // I use 0 for FALSE.
    EML_SUCCESS(STAFCV_OK);
  }
  if(!strcmp(val,"TRUE")||!strcmp(val,"ON")) {
    eml_pretty_on=7; // I use 7 for TRUE.
    EML_SUCCESS(STAFCV_OK);
  }
  EML_CONTEXT("ERROR: I want 'true', 'false', 'show', or nothing.\n");
  EML_FAILURE(INVALID_COMMAND_PARAMETER);
}
void kam_asu_time_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

	STAFCV_T status = asu_time();
}
STAFCV_T asu_time()
{
   struct timeval t, *tp=&t;
   static double t0=0.0;
   double t1;

   if( 1.0 > t0 ){
      gettimeofday(tp,NULL);
      t0=((double)t.tv_sec)+((double)(t.tv_usec)/1000000.);
      t1=t0;
   }
   else {
      gettimeofday(tp,NULL);
      t1=((double)t.tv_sec)+((double)(t.tv_usec)/1000000.);
   }

   set_staf_result((float)(t1-t0));
   printf("ASU:\tTime = %f \n",t1-t0);
   EML_SUCCESS(STAFCV_OK);
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
void kam_asu_date_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = asu_date();
}
STAFCV_T asu_date()
{
   time_t it,*pt=&it;

   *pt = time(0);
   printf("ASU:\tDate = %s \n",ctime(pt));
   EML_SUCCESS(STAFCV_OK);
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
void kam_asu_fflush_()
{
   long npars = ku_npar();      /* number of KUIP parameters */

        STAFCV_T status = asu_fflush();
}
STAFCV_T asu_fflush()
{
   fflush(0);
   EML_SUCCESS(STAFCV_OK);
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
void kam_asumalloc_stats_()
{
	STAFCV_T status = asumalloc_stats();
}
STAFCV_T asumalloc_stats()
{
   asuMallocStats();
   EML_SUCCESS(STAFCV_OK);
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
void kam_asumalloc_level_()
{
   int level = ku_geti();
	STAFCV_T status = asumalloc_level(level);
}
STAFCV_T asumalloc_level(int level)
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

