/*------------------------------------------------------------------
FILE:         ssf_10to8_init.c
DESCRIPTION:  SSF - ASIC Simulator - 10to8 Bit Conversion Initialization
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      May 23, 96 cap ported from sss
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long ssf_10to8_init
DESCRIPTION:  A routine to initialize the 10 bit to 8 bit conversion
              look up table (ssf_10to8_initmap).
ARGUMENTS: [ou]   ssf_10to8_map : 10 to 8 bit mapping table 
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "ssf_10to8_init.h"

long type_of_call ssf_10to8_init_(  
  TABLE_HEAD_ST *map_h,  SSF_MAP_ST *map)
{  
  long i;

  for (i = 0; i < 128; i++)
    map[i].adc = i;
  for (i = 128; i < 384; i++)
    map[i].adc = 128 + (i - 128) / 4;
  for (i = 384; i < 1024; i++)
    map[i].adc = 192 + (i - 384) / 10;
  
  map_h->nok = 1024;

  return STAFCV_OK;
}





