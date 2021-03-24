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
	u_short params ;


	if(geo.ns==0) params = 0xBBAA ;
	else params = 0xDDCC ;

	
	int big_or = 0 ;

	for(int i=0;i<20;i++) {	// 0..19
		for(int j=0;j<8;j++) {

			if(s2_ch_mask[geo.ns] & (1ll<<i)) {
				ecal[i].d[j] = 0 ;
				continue ;
			}

			if(ecal[i].d[j]) big_or |= 1 ;
		}
	}

	for(int i=0;i<8;i++) {	// 20..27
		for(int j=0;j<8;j++) {

			if(s2_ch_mask[geo.ns] & (1ll<<(20+i))) {
				hcal[i].d[j] = 0 ;
				continue ;
			}

			if(hcal[i].d[j]) big_or |= 1 ;
		}
	}

	for(int i=0;i<6;i++) {	// 28..33 
		for(int j=0;j<8;j++) {

			if(s2_ch_mask[geo.ns] & (1ll<<(28+i))) {
				pres[i].d[j] = 0 ;
				continue ;
			}

			if(pres[i].d[j]) big_or |= 1 ;
		}
	}


	output[0].d[0] = params&0xFF;
	output[0].d[1] = pres[2].d[7] ;	// 30
	output[0].d[2] = pres[0].d[6] ;	// 28
	output[0].d[3] = hcal[6].d[5] ;	// 26
	output[0].d[4] = ecal[4].d[2] ;
	output[0].d[5] = ecal[2].d[1] ;
	output[0].d[6] = ecal[0].d[0] ;
//	output[0].d[7] = 0xCD ;
	output[0].d[7] = big_or?0xFD:0x7D ;
	
	output[1].d[0] = params>>8 ;
	output[1].d[1] = pres[3].d[7] ;	// 31
	output[1].d[2] = pres[1].d[6] ;	// 29
	output[1].d[3] = hcal[7].d[5] ;	// 27
	output[1].d[4] = ecal[5].d[2] ;	
	output[1].d[5] = ecal[3].d[1] ;	
	output[1].d[6] = ecal[1].d[0] ;	
//	output[1].d[7] = big_or?0x8B:0x0B ;
	output[1].d[7] = big_or?0x8C:0x0C ;
	
	return ;
}

