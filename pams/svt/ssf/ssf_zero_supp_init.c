/*------------------------------------------------------------------
FILE:         ssf_zero_supp_init.c
DESCRIPTION:  SVT ASIC Simulator - Zero Suppression
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      May 23, 96 cap ported from sss
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long ssf_zero_supp_init
DESCRIPTION:  Initialize Zero Suppresion Parameters
ARGUMENTS: 
ssf_zero_par  [out]   : zero suppression parameters
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "ssf_zero_supp_init.h"

long type_of_call ssf_zero_supp_init_(  
  TABLE_HEAD_ST *zero_par_h,  SSF_ZERO_PAR_ST  *zero_par)
{  
  zero_par->n_anode     = 210;
  zero_par->n_time      = 128;
  zero_par->n_high      = 1;
  zero_par->n_low       = 3;
  zero_par->thresh_high = 2;
  zero_par->thresh_low  = 1;
  zero_par_h->nok = 1;

  return STAFCV_OK;
}

