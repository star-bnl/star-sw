/*------------------------------------------------------------------
FILE:         ssf_10to8.c
DESCRIPTION:  SSF - ASIC Simulator - 10to8 Bit Conversion
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      May 23, 96 cap ported from sss
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long ssf_10to8
DESCRIPTION:  A routine to do 10 bit to 8 bit conversion using
              a look up table (map10to8).
              The routine takes the adc10 values and trasnforms
	      into adc8 values.
ARGUMENTS: [in]   map10to8 : 10 to 8 bit mapping table 
           [in]   adc10    : svt pixel 10 bit data
           [ou]   adc8     : svt_pixel  8 bit data
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "ssf_10to8.h"

long type_of_call ssf_10to8_(  
  TABLE_HEAD_ST *map_h,    SSF_MAP_ST *map,
  TABLE_HEAD_ST *adc10_h,  SSF_ADC_ST *adc10,
  TABLE_HEAD_ST *adc8_h,   SSF_ADC_ST *adc8)
{  
  int i, ipixel, iADC10, iADC8;

  /* Loop on all pixels in table and perform conversion */
  /* The normal range is 0-1023                         */
  /* Underflow indicated by -1                          */
  /* Overflow  indicated by -2                          */
  
  for (ipixel = 0; i < adc10_h->nok; i++)
    {
      iADC10 = adc10[ipixel].adc;
      if (iADC10==-1)              /* ADC under flow indicator */
	iADC8 = -1;
      else if (iADC10==-2)         /* ADC over flow indicator  */
	iADC8 = -2;
      else if (iADC10>1023)
	iADC8 = -2;
      else if (iADC10<0) 
	iADC8 = -1;
      else
	iADC8 = map[iADC10].adc;  /* conversion */

      adc8[ipixel].adc = iADC8;
    }
  adc8_h->nok = adc10_h->nok;

  return STAFCV_OK;
}





