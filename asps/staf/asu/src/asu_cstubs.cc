/*:
*:>---------------------------------------------------------------------
*:FILE:         asu_cstubs.cc
*:DESCRIPTION:  
*:AUTHOR:       Dave Morrison
*:BUGS:        
*:HISTORY:     
*:<---------------------------------------------------------------------
*/
#undef CORBA

/*-------------------------------------------- INCLUDES             --*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#if defined(irix) || defined(irix64) || defined (linux) || defined (__alpha)
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

STAFCV_T 
asu_hello(char* msg)
{
   printf("ASU:\tHello, %s \n",msg);
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
asu_demand_ack(char *value) {
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

STAFCV_T 
asu_beep(char *value) {
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

STAFCV_T 
asu_pretty(char *value) {
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

STAFCV_T 
asu_time()
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

STAFCV_T 
asu_date()
{
   time_t it,*pt=&it;

   *pt = time(0);
   printf("ASU:\tDate = %s \n",ctime(pt));
   EML_SUCCESS(STAFCV_OK);
}

STAFCV_T 
asu_fflush()
{
   fflush(0);
   EML_SUCCESS(STAFCV_OK);
}

