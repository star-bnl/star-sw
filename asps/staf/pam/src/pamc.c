/* This is a C template that you can use to begin
** the coding of your module.  It was made by STAR's idl com-
** piler, which also made a FORTRAN template. */
/*------------------------------------------------------------------
FILE:         pamc.c
DESCRIPTION:  Physics Analysis Module template.
AUTHOR:       hpl - H.P. Lovecraft, hplovecraft@cthulhu.void
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      00jan96-v000a-hpl- Creation.
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long PAM
DESCRIPTION:  Physics Analysis Module template.
ARGUMENTS:    TB1     - table 1
ARGUMENTS:    TB2     - table 2
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "pamc.h"

long pamc_(
  TABLE_HEAD_ST             *t1_h,        SCALARS_ST               *t1 ,
  TABLE_HEAD_ST             *t2_h,        VECTORS_ST               *t2 ) {
int i;
printf(" name = (%20s) nok = (%d) maxlen = (%d)\n"      /*IGNORE*/
        ,t1_h->name,t1_h->nok,t1_h->maxlen);            /*IGNORE*/
printf(" name = (%20s) nok = (%d) maxlen = (%d)\n"      /*IGNORE*/
        ,t2_h->name,t2_h->nok,t2_h->maxlen);            /*IGNORE*/

  for(i=0;i<t1_h->nok;i++){
     t1[i].aShort = i;
     t1[i].aUshort = i;
     t1[i].aLong = i;
     t1[i].aUlong = i;
     t1[i].aChar = '-';
     t1[i].aOctet = i;
     t1[i].aFloat = i;
     t1[i].aDouble = i;
  }

  return STAFCV_OK;
}
