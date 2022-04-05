#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"
#include "fcs_ecal_epd_mask.h"

/*
	Tonko's test code used in Apr+ 2021


	Will be stage_version 0xFF210204
*/

// 20 ECAL links
// 8 HCAL link
// 6 FPRE links
// 2 output links
void  fcs_trg_base::stage_2_tonko_202104(link_t ecal[], link_t hcal[], link_t pres[], geom_t geo, link_t output[])
{    

	int ecal_or = 0 ;
	int hcal_or = 0 ;
	int fpre_or = 0 ;

	for(int i=0;i<20;i++) {	// 0..19
		for(int j=0;j<8;j++) {

			if(s2_ch_mask[geo.ns] & (1ll<<i)) {
				ecal[i].d[j] = 0 ;
				continue ;
			}

			if(ecal[i].d[j]) ecal_or |= 1 ;
		}
	}

	for(int i=0;i<8;i++) {	// 20..27
		for(int j=0;j<8;j++) {

			if(s2_ch_mask[geo.ns] & (1ll<<(20+i))) {
				hcal[i].d[j] = 0 ;
				continue ;
			}

			if(hcal[i].d[j]) hcal_or |= 1 ;
		}
	}

	for(int i=0;i<6;i++) {	// 28..33 
		for(int j=0;j<8;j++) {

			if(s2_ch_mask[geo.ns] & (1ll<<(28+i))) {
				pres[i].d[j] = 0 ;
				continue ;
			}

			if(pres[i].d[j]) fpre_or |= 1 ;
		}
	}

	// lower 8 bits
	output[0].d[0] = ecal_or?0xEE:0 ;
	output[0].d[1] = hcal_or?0xAA:0 ;
	output[0].d[2] = fpre_or?0xFF:0 ;
	output[0].d[3] = 0 ;
	output[0].d[4] = 0 ;
	output[0].d[5] = 0 ;
	output[0].d[6] = (fpre_or<<2)|(hcal_or<<1)|ecal_or ;
	output[0].d[7] = 0xCD ;

	// upper 8 bits
	output[1].d[0] = 0 ;
	output[1].d[1] = 0 ;
	output[1].d[2] = 0 ;
	output[1].d[3] = 0 ;
	output[1].d[4] = 0 ;
	output[1].d[5] = 0 ;
	output[1].d[6] = 0 ;
	output[1].d[7] = 0xAB ;

	
	
	return ;
}

