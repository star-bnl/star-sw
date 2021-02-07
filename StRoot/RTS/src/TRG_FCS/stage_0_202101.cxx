
#include <stdio.h>
#include "fcs_trg_base.h"

// Running in FY21 as V01

void fcs_trg_base::stage_0_202101(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *dta_out)
{
	int sum = 0 ;
	int peak = 0 ;
	int last = 0 ;

	for(int tb=0;tb<8;tb++) {
		u_short radc = adc.d[tb] ;

		switch(tb) {
		case 0 :
			sum = radc ;

			//printf("... IN S0: tb 0 = %d\n",sum) ;

			peak = 0 ;
			last = 0 ;
			break ;
		default :
			sum += radc ;
			if(tb==7) {
				//printf("... IN S0: %d (%d,%d)\n",sum,pg->ped,pg->gain) ;

				sum -= pg->ped ;
				if(sum < 0) sum = 0 ;
 				if(sum>0 && fcs_trgDebug>=2){ 
				    if(geo.det<2){
					printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d pT=%6.3f\n",
					       geo.ns,geo.det,geo.dep,geo.ch,
					       sum,pg->gain,(sum*pg->gain)>>6,
					       0.00024711*((sum*pg->gain)>>6) );
				    }else{
					printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d MIP=%5.3f\n",
					       geo.ns,geo.det,geo.dep,geo.ch,
					       sum,pg->gain,(sum*pg->gain)>>6,
					       float((sum*pg->gain)>>6)/100.0);
				    }					
				}
				sum *= pg->gain ; // note that in FY21+ gain==1.0 is 0x100
				sum >>= 8 ;		// see note above

				
			}
		}
	}

	//printf("... IN S0: %d\n",sum) ;

	*dta_out = sum ;
}
