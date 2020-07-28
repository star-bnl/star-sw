#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg.h"

// Stage 0 is where the raw ADCs get summed, pedestal subtracted, gain corrected.
// This I expect to do within the framework.
// The example below should be pretty finished.
 
int stage_0(short radc, int timebin, geom_t geo, ped_gain_t *pg, u_int *dta_out)
{
	static int sum ;
	static int peak ;
	static int last ;
	
	switch(timebin) {
	case 0 :
		sum = radc ;	// reset 
		peak = 0 ;	// reset
		last = 0 ;	// reset 
		break ;	
	case 1 :
		sum += radc ;

		break ;
	case 2 :
		sum += radc ;

		last = radc ;	// cache tb==2
		
		break ;
	case 3 :		// assume this is where the peak should fall
		sum += radc ;

		if(radc >= last) peak = 1 ;	// rising part

		last = radc ;	// cache also tb==3

		break ;
	case 4 :
		sum += radc ;

		if(radc < last) peak |= 2 ;	// falling part

		break ;
	case 5 :
		sum += radc ;

		break ;
	case 6 :
		sum += radc ;

		break ;
	case 7 :
		sum += radc ;
		int sumkeep = sum;

		sum -= pg->ped ;	// subtract pedestal
		if(sum < 0) sum = 0 ;	// no underflow allowed

		sum *= pg->gain ;	// gain & pt correction -- note fixed point multiplication with 4.6f

		sum >>= 6 ;		// back to 19 bits
 
		if(fcs_trgDebug>=4 && sumkeep>0){
		    printf("---> %1d:%1d:%2d:%2d : peak=%1d ped=%4d gain=%6.4f rsum=%5d correctedSum=%5d\n",
			   geo.ns,geo.det,geo.dep,geo.ch,
			   peak,pg->ped,(float)pg->gain/64.0,sumkeep,sum) ;
	}

		break ;
	}

	if(timebin==7 && peak==3) *dta_out = sum ;	// valid datum!!!
	else *dta_out = 0 ;

	return 0 ;
}

