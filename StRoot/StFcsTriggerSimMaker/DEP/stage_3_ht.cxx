#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg.h"


// And the last stage where North and South are combined.
// Output is a 12 bit value (max) which goes into STAR Trigger
// either LastDSM, RAT, TCU, etc.

int stage_3_ht(link_t link[4], u_short *dsm_out)
{
	// High Tower trigger example

	u_int ecal_adc[2] ;
	u_int max_ecal_adc ;

	ecal_adc[0] = (link[0].d[2]<<16) | (link[0].d[1]<<8) | link[0].d[0] ;
	ecal_adc[1] = (link[2].d[2]<<16) | (link[2].d[1]<<8) | link[2].d[0] ;


	if(ecal_adc[0]>ecal_adc[1]) {
		max_ecal_adc = ecal_adc[0] ;
	}
	else {
		max_ecal_adc = ecal_adc[1] ;
	}

//	printf("stage_3: max ECAL ADC %d\n",max_ecal_adc) ;

	*dsm_out = 0 ;


	// for an example, hardcoded High Tower Threshods: 10,100,1000
	if(max_ecal_adc > 10) *dsm_out |= 1 ;
	if(max_ecal_adc > 100) *dsm_out |= 2 ;
	if(max_ecal_adc > 1000) *dsm_out |= 4 ;

	return 0 ;
}

