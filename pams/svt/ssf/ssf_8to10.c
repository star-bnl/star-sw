/*------------------------------------------------------------------
FILE:         ssf_8to10.c
DESCRIPTION:  SSF - ASIC Simulator - 8to10 Bit Conversion
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      May 23, 96 cap ported from sss
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long ssf_8to10
DESCRIPTION:  A routine to do 8 bit to 10 bit conversion using
              a look up table (map).
              The routine takes the adc10 values and trasnforms
	      into adc8 values. The look up table must be created with
              ssf_8to10_init.c

ARGUMENTS: [in]   map8to10 : 8 to 10 bit mapping table 
           [in]   adc8     : svt pixel  8 bit data
           [ou]   adc10    : svt_pixel 10 bit data
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "ssf_8to10.h"

long type_of_call ssf_8to10_(  
  TABLE_HEAD_ST *map_h,    SSF_MAP_ST *map,
  TABLE_HEAD_ST *adc8_h,   SSF_ADC_ST *adc8,
  TABLE_HEAD_ST *adc10_h,  SSF_ADC_ST *adc10)
{  
  int i, ipixel, iADC10, iADC8;

  /* Loop on all pixels in table and perform conversion */
  /* The normal range is 0-1023                         */
  /* Underflow indicated by -1                          */
  /* Overflow  indicated by -2                          */
  
  for (ipixel = 0; i < adc8_h->nok; i++)
    {
      iADC8 = adc8[ipixel].adc;
      if (iADC8==-1)              /* ADC under flow indicator */
	iADC10 = -1;
      else if (iADC8==-2)         /* ADC over flow indicator  */
	iADC10 = -2;
      else if (iADC8>255)
	iADC10 = -2;
      else if (iADC8<0) 
	iADC10 = -1;
      else
	iADC10 = map[iADC8].adc;  /* conversion */

      adc10[ipixel].adc = iADC10;
    }
  adc10_h->nok = adc8_h->nok;

  return STAFCV_OK;
}


