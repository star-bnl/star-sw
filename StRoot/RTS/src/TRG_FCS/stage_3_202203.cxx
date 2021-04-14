#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"

// And the last stage where North and South are combined.
// For run21, output are 9 bits
// it goes first into the RAT and then to the LastDSM

void fcs_trg_base::stage_3_202203(link_t link[], u_short *dsm_out)
{
    *dsm_out = 0;

    if( (link[0].d[6] & 0x01) || (link[2].d[6] & 0x01) ) *dsm_out |= 0x1;   //EcalHT FCS0
    if( (link[0].d[6] & 0x02) || (link[2].d[6] & 0x02) ) *dsm_out |= 0x2;   //HcalHT FCS1
    if( (link[0].d[5] & 0x01) || (link[2].d[5] & 0x01) ) *dsm_out |= 0x4;   //ETOT   FCS2
    if( (link[0].d[5] & 0x02) || (link[2].d[5] & 0x02) ) *dsm_out |= 0x8;   //HTOT   FCS3
    if( (link[0].d[4] & 0x01) || (link[2].d[4] & 0x01) ) *dsm_out |= 0x10;  //JP1    FCS4
    if( (link[0].d[0] & 0x01) || (link[2].d[0] & 0x01) ) *dsm_out |= 0x20;  //EM1    FCS5
    if( (link[0].d[3] & 0x01) || (link[2].d[3] & 0x01) ) *dsm_out |= 0x40;  //HAD1   FCS6
    if( (link[0].d[2] & 0x01) || (link[2].d[2] & 0x01) ) *dsm_out |= 0x80;  //GAM1   FCS7
    if( (link[0].d[1] & 0x01) || (link[2].d[1] & 0x01) ) *dsm_out |= 0x100; //ELE1   FCS8
   
    dsmout = *dsm_out;

    if(fcs_trgDebug>=1) {
	printf("FCS STG3 input 0 = %02x %02x %02x %02x %02x %02x %02x %02x\n",
	       link[0].d[0],link[0].d[1],link[0].d[2],link[0].d[3],
	       link[0].d[4],link[0].d[5],link[0].d[6],link[0].d[7]);
	printf("FCS STG3 input 1 = %02x %02x %02x %02x %02x %02x %02x %02x\n",
	       link[2].d[0],link[2].d[1],link[2].d[2],link[2].d[3],
	       link[2].d[4],link[2].d[5],link[2].d[6],link[2].d[7]);
	printf("FCS STG3 output = %04x = ", *dsm_out);
	for(int i=7; i>=0; i--){printf("%1d", (dsmout>>i)&0x1);}
	printf("\n");	    
    }
}
