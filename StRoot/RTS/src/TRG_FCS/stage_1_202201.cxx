#include <stdio.h>	// for debugging/logging

#include "fcs_trg_base.h"

// VERSION 0x1  (May 2020)
void fcs_trg_base::stage_1_202201(u_int s0[], geom_t geo, link_t *output) 
{
         if(fcs_trgDebug>=2) printf("Stage1v1 ns=%1d det=%1d dep=%2d\n",geo.ns,geo.det,geo.dep);

	// Tonko: I will run this basic "high-towerish" trigger always
	u_char t[32] ;
	u_int mask = 0 ;
	u_int thr = ht_threshold[geo.det] ;	// depends on detector
	
	for(int i=0;i<32;i++) {
		if(s0[i] > thr) {
			mask |= (1<<i) ;
			t[i] = 1 ;
		}
		else {
			mask &= ~(1<<i) ;
			t[i] = 0 ;
		}
	}

	// algorithm depends on detector
	if(geo.det == 2) {	// BASIC algo for fPRE
	    if(fcs_trgDebug>0){
		for(int ch=0; ch<32; ch++) {
		    padc[geo.ns][geo.dep][ch]=s0[ch]; //keeping them for QA
		}
	    }
	    output->d[0] = (t[ 7]<<7)|(t[ 6]<<6)|(t[ 5]<<5)|(t[ 4]<<4)|(t[ 3]<<3)|(t[ 2]<<2)|(t[ 1]<<1)|(t[ 0]<<0);
	    output->d[1] = (t[15]<<7)|(t[14]<<6)|(t[13]<<5)|(t[12]<<4)|(t[11]<<3)|(t[10]<<2)|(t[ 9]<<1)|(t[ 8]<<0);
	    output->d[2] = (t[23]<<7)|(t[22]<<6)|(t[21]<<5)|(t[20]<<4)|(t[19]<<3)|(t[18]<<2)|(t[17]<<1)|(t[16]<<0);
	    output->d[3] = (t[31]<<7)|(t[30]<<6)|(t[29]<<5)|(t[28]<<4)|(t[27]<<3)|(t[26]<<2)|(t[25]<<1)|(t[24]<<0);
	    output->d[4] = 0 ;
	    output->d[5] = 0 ;
	    output->d[6] = 0 ;
	    output->d[7] = mask?0x80:0 ;
	}
	else {	// BASIC algo for ECAL/HCAL
		u_int sum[8] ;

		// Akio
		sum[0] = s0[0] + s0[1] + s0[4] + s0[5] ;
		sum[1] = s0[2] + s0[3] + s0[6] + s0[7] ;
		sum[2] = s0[8] + s0[9] + s0[12] + s0[13] ;
		sum[3] = s0[10] + s0[11] + s0[14] + s0[15] ;
		sum[4] = s0[16] + s0[17] + s0[20] + s0[21] ;
		sum[5] = s0[18] + s0[19] + s0[22] + s0[23] ;
		sum[6] = s0[24] + s0[25] + s0[28] + s0[29] ;
		sum[7] = s0[26] + s0[27] + s0[30] + s0[31] ;
	
		// outputs to stage_2
		for(int i=0;i<8;i++) {
			//fprintf(stderr,"SUM %d: %d: %d\n",i,sum[i],sum[i]>>7) ;

			//printf(" IN S1: ix %d: %d\n",i,sum[i]) ;

		        //if(sum[i]>131071) output->d[i] = 0xFF ;
		        if(sum[i]>0x7FFF) output->d[i] = 0xFF ;
			else output->d[i] = sum[i]>>7 ;

			// Tonko: Akio has a bug here because sum values between 132k and 32k
			// will overflow the 8 bits.
			// I fixed this to match the curreant VHDL:
			//
			// Akio - My original code was
			//    if(sum[i] > 0x7fff) sum[i]=0x7fff;
                        //    output->d[i] = sum[i] >> 7;   
			// and where was the bug?
			//
                        // 131071 is 0x1ffff and 131071>>7 is 0x3ff
			// It seems to be inconsistent - akio

			output->d[i] &= 0xFF ;

			//printf("  IN S1: ix %d: %d out\n",i,output->d[i]) ;

		}
	}
}
