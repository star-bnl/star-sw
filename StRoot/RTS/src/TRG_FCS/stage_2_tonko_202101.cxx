#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"
#include "fcs_ecal_epd_mask.h"

/*
	Tonko's test code used in Feb+ 2021


	Will be stage_version 0xFF210201
*/

// 20 ECAL links
// 8 HCAL link
// 6 FPRE links
// 2 output links
void  fcs_trg_base::stage_2_tonko_202101(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[])
{    

	int big_or = 0 ;

	for(int i=0;i<20;i++) {
		for(int j=0;j<8;j++) {
			if(ecal[i].d[j]) big_or |= 1 ;
		}
	}

	for(int i=0;i<8;i++) {
		for(int j=0;j<8;j++) {
			if(hcal[i].d[j]) big_or |= 1 ;
		}
	}

	for(int i=0;i<6;i++) {
		for(int j=0;j<8;j++) {
			if(pres[i].d[j]) big_or |= 1 ;
		}
	}


	output[0].d[0] = stage_params[2][0]&0xFF;
	output[0].d[1] = ecal[0].d[0] ;
	output[0].d[7] = 0xCD ;
	
	output[1].d[0] = stage_params[2][0]>>8 ;
	output[1].d[1] = ecal[1].d[0] ;
	output[1].d[7] = big_or?0x8B:0x0B ;
	
	return ;
}

