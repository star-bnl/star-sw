#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg.h"

// First stage Trigger algorithm: local to any DEP board.
// Inputs are 32 bit ADC values, output is what gets sent on the outgoing link.
// Processing is detector dependent thus one expects to use the geometry.

int stage_1_ht(u_int adc[32], geom_t geo, link_t *output)
{
	//simple high tower example

	u_int max_i = 0 ;
	u_int max = 0 ;


	for(int i=0;i<32;i++) {
		if(adc[i] > max) {
			max = adc[i] ;
			max_i = i ;
		}
	}

//	printf("stage_1: %d:%d:%d = max_i %d = %d max\n",geo.ns,geo.det,geo.dep,max_i,max) ;

	output->d[0] = max & 0xFF ;
	output->d[1] = (max >> 8) & 0xFF ;
	output->d[2] = (max >> 16) & 0xFF ;
	output->d[3] = (max >> 24) & 0xFF ;
	output->d[4] = max_i ;	
	output->d[5] = geo.dep ;
	output->d[6] = geo.ns ;
	output->d[7] = 0xAB ;

	return 0 ;
}

