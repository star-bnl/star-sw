/*------------------------------------------------------------------
FILE:         ssf_8to10_init.c
DESCRIPTION:  ssf - Initialize the inverse of the compression table
AUTHOR:       C.Pruneau,Dave Read
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      Aug 18, 1996
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long ssf_8to10_init
DESCRIPTION:  Initialize the inverse mapping table ssf_8to10map

The compression map is a one-to-one lookup table. It is used to define
the expansion table, also a one-to-one lookup table, by looping on
all possible values from 1-256 (8bits).

The compression table is initialized by the routine ssf_10to8_init.

ARGUMENTS:    ssf_10to8map  : compression table (input)
ARGUMENTS:    ssf_8to10map  : expension table (output)
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "ssf_8to10_init.h"

long type_of_call ssf_8to10_init_(
		     TABLE_HEAD_ST  *map10to8_h, SSF_MAP_ST *map10to8,
		     TABLE_HEAD_ST  *map8to10_h, SSF_MAP_ST *map8to10) 
{

  int i8, i10;
  int i8max  = 256;
  int i10max = 1024;

  if (map10to8_h->nok<=0) 
    {
      printf("ssf_8to10_init-E1 : map10to8_h->nok<=0 \n");
      return STAFCV_BAD;
    }
  
  for (i8 = 0; i8 < i8max; i8++)
    for (i10 = 0; i10 < i10max; i10++)
      if (map10to8[i10].adc == i8)
	map8to10[i8].adc = i10;
  
  map8to10_h->nok = i8max;
  
  return STAFCV_OK;
}
