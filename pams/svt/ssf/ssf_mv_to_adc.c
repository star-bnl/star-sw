/*------------------------------------------------------------------
FILE:         ssf_mv_to_adc.c
DESCRIPTION:  SSF - ADC Simulator: millivolts to ADC
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      May 23, 96 cap ported from sss
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long ssf_mv_to_adc
DESCRIPTION:  A routine to simulate the ADC conversion
              of millivolts into 10 bits ADC values
ARGUMENTS: [in]   mv    : svt pixel data in milli-volts
           [ou]   adc10 : svt pixel data in 10 bits ADC
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "ssf_mv_to_adc.h"

long type_of_call ssf_mv_to_adc_(  
  TABLE_HEAD_ST *mv_h,     SSF_MV_ST  *mv,
  TABLE_HEAD_ST *adc10_h, SSF_ADC_ST  *adc10)
{  
  int ipixel, iADC10;
  float current_mv;
  float ADC_SENSITIVITY = 2.0; /* counts per millivolt */
  float ADC_PEDESTAL = 10.;    /* arbitrary pedestal   */

  /* Loop on all pixels in table and perform A-D conversion */
  /* The normal range is 0-1023                         */
  /* Underflow indicated by -1                          */
  /* Overflow  indicated by -2                          */
  
  for (ipixel = 0; ipixel < mv_h->nok; ipixel++)
    {
      iADC10 = ADC_PEDESTAL + ADC_SENSITIVITY*mv[ipixel].mv;

      if (iADC10<0) 
	iADC10 = -1;             /* ADC underflow */
      else if (iADC10>1023)
	iADC10 = -2;             /* ADC overflow  */

      adc10[ipixel].adc = iADC10;
    }

  adc10_h->nok = mv_h->nok;

  return STAFCV_OK;
}
