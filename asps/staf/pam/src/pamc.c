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

#define MAX(A,B) (A>B ? A : B)
#define MIN(A,B) (A<B ? A : B)

#include "pamc.h"
/*- PROTOTYPES -*/
extern int ds2ReallocTable(TABLE_HEAD_ST** ppHead,char** ppData
		, size_t newCount);
int ds2reallloctable_(TABLE_HEAD_ST** ppHead,char** ppData
                , size_t newCount);

/*------------------------------------------------------------------
ROUTINE:      long PAM
DESCRIPTION:  Physics Analysis Module template.
ARGUMENTS:    TB1     - table 1
ARGUMENTS:    TB2     - table 2
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/

long pamc_(
  TABLE_HEAD_ST             *t1_h,        SCALARS_ST               *t1 ,
  TABLE_HEAD_ST             *t2_h,        VECTORS_ST               *t2 ) {
int i,j;

printf("########################################\n");
printf("###  pamc - fill tables \n");
printf("########################################\n");

if( !(0 < t1_h->maxlen) ){
  ds2ReallocTable(&t1_h,(char**)&t1,100);
  t1_h->maxlen = 100;
  t1_h->nok = 10;
}
if( !(0 < t2_h->maxlen) ){
  ds2ReallocTable(&t2_h,(char**)&t2,10);
  t2_h->maxlen = 10;
  t2_h->nok = 10;
}

printf(" name = (%20s) nok = (%d) maxlen = (%d)\n"      /*IGNORE*/
        ,t1_h->name,t1_h->nok,t1_h->maxlen);            /*IGNORE*/
printf(" name = (%20s) nok = (%d) maxlen = (%d)\n"      /*IGNORE*/
        ,t2_h->name,t2_h->nok,t2_h->maxlen);            /*IGNORE*/

  for(i=0;i<(t1_h->nok);i++){
     t1[i].aShort = i;
     t1[i].aUshort = i;
     t1[i].aLong = i;
     t1[i].aUlong = i;
     t1[i].aChar = '-';
     t1[i].aOctet = i;
     t1[i].aFloat = i;
     t1[i].aDouble = i;
  }
  for(i=0;i<(t2_h->nok);i++){
     for(j=0;j<3;j++){
	t2[i].bShorts[j] = i+j;
	t2[i].bUshorts[j] = i+j;
	t2[i].bLongs[j] = i+j;
	t2[i].bUlongs[j] = i+j;
	t2[i].bChars[j] = '-';
	t2[i].bOctets[j] = i+j;
	t2[i].bFloats[j] = i+j;
	t2[i].bDoubles[j] = i+j;
     }
  }

  return STAFCV_OK;
}

