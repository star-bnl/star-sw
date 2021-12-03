#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "fcs_trg_base.h"
#include "fcs_ecal_epd_mask.h"

/*
	Tonko's test code used in Feb+ 2021


	Will be stage_version 0xFF210201
*/

// 4 stage2 links: 2 north, 2 south
// 1 DSM output
// 6 FPRE links

static int or_reduce(int d8)
{
	if(d8) return 1 ;
	return 0 ;
}

void  fcs_trg_base::stage_3_tonko_202101(link_t s2[], uint16_t *dsm)
{    
 	uint16_t out_s ;
	uint16_t out_n ;
	uint16_t n_or = 0 ;
	uint16_t s_or = 0 ;
	uint16_t big_or = 0 ;

	out_s = 0 ;
	out_n = 0 ;

	for(int i=0;i<8;i++) {
		out_n |= (or_reduce(s2[0].d[i]) || or_reduce(s2[1].d[i]))<<i ;
		out_s |= (or_reduce(s2[2].d[i]) || or_reduce(s2[3].d[i]))<<i ;
	}

	big_or = s2[0].d[7] & 0x80 ;
	big_or |= s2[1].d[7] & 0x80 ;
	big_or |= s2[2].d[7] & 0x80 ;
	big_or |= s2[3].d[7] & 0x80 ;

	if(big_or) big_or = 1 ;
	else big_or = 0 ;

	n_or = s2[0].d[7] & 0x80 ;
	n_or |= s2[1].d[7] & 0x80 ;
	s_or = s2[2].d[7] & 0x80 ;
	s_or |= s2[3].d[7] & 0x80 ;

	if(n_or) n_or = 1 ;
	if(s_or) s_or = 1 ;

	
	out_n &= 0xF8 ;
	out_n |= (s_or<<2)|(n_or<<1)|big_or ;	// bit(0) is a HT-like indicator


	int sel = (stage_params[3][0]>>1)&0x7 ;
	
	switch(sel) {
	case 0 :
		*dsm = (out_s<<8)|out_n ;
		break ;
	case 3 :
		*dsm = stage_params[3][1] ;
		break ;
	default :
		*dsm = 0xDEAD ;
		break ;
	}

#if 0
	for(int i=0;i<8;i++) {
		printf("%d: 0x%X 0x%X 0x%X 0x%X\n",i,s2[0].d[i],s2[1].d[i],s2[2].d[i],s2[3].d[i]) ;
	}
	printf("  sel is %d, dsm is 0x%04X\n",sel,*dsm) ;
#endif
	return ;
}

