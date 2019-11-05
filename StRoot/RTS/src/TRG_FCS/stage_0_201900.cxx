
#include "fcs_trg_base.h"

void fcs_trg_base::stage_0_201900(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *dta_out)
{
	int sum = 0 ;
	int peak = 0 ;
	int last = 0 ;

	for(int tb=0;tb<8;tb++) {
		u_short radc = adc.d[tb] ;

		switch(tb) {
		case 0 :
			sum = radc ;
			peak = 0 ;
			last = 0 ;
			break ;
		default :
			sum += radc ;
			if(tb==7) {

				sum -= pg->ped ;
				if(sum < 0) sum = 0 ;

				sum *= pg->gain ;

				sum >>= 6 ;
			}
		}
	}

	*dta_out = sum ;

}
