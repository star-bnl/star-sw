#include <stdio.h>
#include <sys/types.h>
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

void  fcs_trg_base::stage_3_tonko_202101(link_t s2[], u_short *dsm)
{    
	u_short out_s ;
	u_short out_n ;

	out_s = 0 ;
	out_n = 0 ;

	for(int i=0;i<8;i++) {
		out_n |= (or_reduce(s2[0].d[i]) || or_reduce(s2[1].d[i]))<<i ;
		out_s |= (or_reduce(s2[2].d[i]) || or_reduce(s2[3].d[i]))<<i ;
	}

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

	return ;
}

