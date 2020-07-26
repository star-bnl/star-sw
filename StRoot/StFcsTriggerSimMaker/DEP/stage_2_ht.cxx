#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg.h"



// Processing on the North or South DEP/IO flavoured board. 
// Inputs are up to 32 links but I already organized them according to strawman.
// output is 1 link over the external OUT connector.
// Right now we assume we have 20 inputs from ECAL, 6 from HCAL and 4 from PRE.
// We also assume there is no need to know if this is North or South as
// the processing is exactly the same. Right??

int stage_2_ht(link_t ecal[20], link_t hcal[8], link_t pre[4], geom_t geo, link_t output[2])
{
	// Simple High Tower algorithm for test

	u_int max_ecal_adc ;
	u_int max_ecal_id ;

	max_ecal_adc = 0 ;
	max_ecal_id = 0 ;

	for(int i=0;i<20;i++) {
		u_int adc = ecal[i].d[0] | (ecal[i].d[1]<<8) | (ecal[i].d[2]<<16) | (ecal[i].d[3]<<24) ;
		if(adc > max_ecal_adc) {
			max_ecal_adc = adc ;
			max_ecal_id = ecal[i].d[5]*32+ecal[i].d[4] ;	// linearized channel
		}
	}

	//to 16 bits
//	printf("stage_2: NS %d: ecal id %d, val %d\n",geo.ns,max_ecal_id,max_ecal_adc) ;

	memset(output,0,sizeof(output)) ;

	output[0].d[0] = max_ecal_adc & 0xFF ;
	output[0].d[1] = (max_ecal_adc >> 8) & 0xFF ;
	output[0].d[2] = (max_ecal_adc >> 16) & 0xFF ;
	output[0].d[7] = 0x12 ;	// debugging

	return 0 ;
}

