/*------------------------------------------------------------------
FILE:         ssf_zero_supp.c
DESCRIPTION:  SVT ASIC Simulator - Zero Suppression
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      May 23, 96 cap ported from sss
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long ssf_zero_supp
DESCRIPTION:  Simulate SVT ASIC zero suppression of the Data
ARGUMENTS: 
zero_par  [in]   : zero suppression parameters
adc8      [in]   : wafer pixel values in ADC channel
seq       [ou]   : sequence zbove threshold
adc       [ou]   : adc values for sequences above threshold
RETURN VALUE: STAF Condition Value
IMPLEMENTATION NOTE:
  The output table adc has the same internal structure as the
adc8 table. However, it cannot exists on its own because
it is effectively zero suppressed and needs the geographical 
information contained in the seq table to provide the
geographical addresses of the values stored in the adc table.


------------------------------------------------------------------*/
#include "ssf_zero_supp.h"

long type_of_call ssf_zero_supp_(  
  TABLE_HEAD_ST *zero_par_h,  SSF_ZERO_PAR_ST  *zero_par,
  TABLE_HEAD_ST *adc8_h,           SSF_ADC_ST  *adc8,
  TABLE_HEAD_ST *seq_h,            SSF_SEQ_ST  *seq,
  TABLE_HEAD_ST *adc_h,            SSF_ADC_ST  *adc)
{  
  int  iAcqStart,  iAcqEnd, iRunLength, iHits, i, j; 
  int  iAnode, iAboveHighThresh;

  /* Get the essentials */

  int  Thresh_Low  = zero_par->thresh_low;
  int  Thresh_High = zero_par->thresh_high;
  int  N_Low       = zero_par->n_low;
  int  N_High      = zero_par->n_high;
  int  N_Anode     = zero_par->n_anode;
  int  N_Time      = zero_par->n_time;
  int  index;
  int  iIndexStart, iADC, iADC8;

  /* Set things up for the processing */
  
  iHits      = 0;
  iAcqStart  = iAcqEnd = -1;
  iRunLength = 0;
  iAboveHighThresh = 0;
  iADC = 0;

  for (iAnode = 0; iAnode < N_Anode; iAnode++)
    {
      index = iAnode * N_Time;
      
      for (i = 0; i < N_Time; i++)
	{
	  /* Continue if ADC value is below the acquisition threshhold */
	  
	  if (adc8[index].adc < Thresh_Low)
	    {
	      index++;
	      continue;
	    }
	  
	  iAcqStart = i;
	  iIndexStart = index;
	  
	  /*  Now that we have something above the low threshhold,
	   *  run until the ADC value falls below the low thresh.
	   *  If we encounter an ADC value along the way which is above
	   *  the high threshhold, remember that fact.
	   */
	  
	  while (adc8[index].adc >= (unsigned char) Thresh_Low
		 && i < N_Time)
	    {
	      /* If we're above the high threshhold, increment the count. */
	      
	      if (adc8[index].adc >= Thresh_High)
		iAboveHighThresh++;
	      
	      index++;
	      i++;
	    }
	  
	  i--;      /* Kludge to fix the hanging i++ from the previous loop */
	  iAcqEnd = i;
	  iRunLength = iAcqEnd - iAcqStart + 1;
	  
	  if (iRunLength >= N_Low && iAboveHighThresh > N_High)
	    {
	      /*  Set up sequence data. */
	      
	      if (iHits<seq_h->maxlen && (iADC+iRunLength)<adc_h->maxlen) 
		{
		  seq[iHits].t_beg     = iAcqStart;
		  seq[iHits].t_end     = iAcqEnd;
		  seq[iHits].key_adc   = iADC;
		  seq[iHits].anode_id  = iAnode;
		  iHits++;
		  
		  iADC8 = iAnode*N_Time + iAcqStart;
		  for (j=0;j<iRunLength;j++)
		    {
		      adc[iADC].adc = adc8[iADC8].adc;
		      iADC8++;
		      iADC++;
		    }
		}
	      
	      iAcqStart = iAcqEnd = -1;
	      iRunLength = iAboveHighThresh = 0;
	    }
	}
      iAcqStart = iAcqEnd = -1;
      iRunLength = iAboveHighThresh = 0;
    }
  
  seq_h->nok = iHits;
  adc_h->nok = iADC;
  
  return STAFCV_OK;
}

