#include <stdio.h>
#include "fcs_trg_base.h"


/*
	Changed from 8-timebin sum to highest ADC + 1 closes timebin.

	Also added simple peak-finding -- the maximum of the data
	can't be on timebin 0 or 7.

*/

// Running in FY21 as V02

void fcs_trg_base::stage_0_202103(adc_tick_t adc, geom_t geo, ped_gain_t *pg, u_int *dta_out)
{
	int sum = 0 ;
	int peak = 0 ;
	int last = 0 ;

	for(int tb=0;tb<8;tb++) {
		u_short radc = adc.d[tb] ;

		switch(tb) {
		case 0 :
			last = radc ;
			sum = radc ;
			peak = 0 ;
			break ;
		case 1 :
			if(radc>sum) peak |= 1 ;
			sum += radc ;
			last = radc ;
			break ;
		case 2 :
			sum += radc ;
			last = radc ;
			break ;
		case 3 :
			sum += radc ;
			last = radc ;
			break ;
		case 4 :
			sum += radc ;
			last = radc ;
			break ;
		case 5 :
			sum += radc ;
			last = radc ;
			break ;
		case 6 :
			sum += radc ;
			last = radc ;
			break ;
		case 7 :
		default :
			//printf("radc %d, last %d, peak %d\n",radc,last,peak) ;

			if(radc>=last && peak==0) {
				sum = 0 ;
			}
			else {
				sum += radc ;

				sum -= pg?pg->ped:0 ;	// ped is now only 3*ch_ped!
				if(sum < 0) sum = 0 ;

				if(sum>0 && fcs_trgDebug>=2){ 
					if(geo.det<2){
						printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d pT=%8.5f\n",
						       geo.ns,geo.det,geo.dep,geo.ch,
						       sum,pg->gain,(sum*pg->gain)>>8,
						       0.00024711*((sum*pg->gain)>>8) );
					}else{
						printf("ns=%1d det=%1d dep=%2d ch=%2d sum=%6d gain=%6d s*g=%6d MIP=%5.3f\n",
						       geo.ns,geo.det,geo.dep,geo.ch,
						       sum,pg->gain,(sum*pg->gain)>>8,
						       float((sum*pg->gain)>>8)/100.0);
					}					
				}

				sum *= pg?pg->gain:0x100 ; // note that in FY21+ gain==1.0 is 0x100
				sum >>= 8 ;		   // see note above
			       
			}

			break ;
		}
	}

	//printf("... IN S0: %d\n",sum) ;

	*dta_out = sum ;
}
