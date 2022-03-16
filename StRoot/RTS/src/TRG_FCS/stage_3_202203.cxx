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

    if( (link[0].d[6] & 0x01) || (link[2].d[6] & 0x01) ) *dsm_out |= 0x1;    //EcalHT FCS0
    if( (link[0].d[6] & 0x02) || (link[2].d[6] & 0x02) ) *dsm_out |= 0x2;    //HcalHT FCS1
    if( (link[0].d[5] & 0x01) || (link[2].d[5] & 0x01) ) *dsm_out |= 0x4;    //ETOT   FCS2
    if( (link[0].d[5] & 0x02) || (link[2].d[5] & 0x02) ) *dsm_out |= 0x8;    //HTOT   FCS3
    if( (link[0].d[4] & 0x02) || (link[2].d[4] & 0x02) ) *dsm_out |= 0x10;   //JP2    FCS4
    if( (link[0].d[0] & 0x02) || (link[2].d[0] & 0x02) ) *dsm_out |= 0x20;   //EM2    FCS5
    if( (link[0].d[3] & 0x02) || (link[2].d[3] & 0x02) ) *dsm_out |= 0x40;   //HAD2   FCS6
    if( (link[0].d[2] & 0x02) || (link[2].d[2] & 0x02) ) *dsm_out |= 0x80;   //GAM2   FCS7
    if( (link[0].d[1] & 0x02) || (link[2].d[1] & 0x02) ) *dsm_out |= 0x100;  //ELE2   FCS8
    if( (link[0].d[4] & 0x01) && (link[2].d[4] & 0x01) ) *dsm_out |= 0x200;  //DiJP1  FCS9
    if( (link[0].d[0] & 0x01) && (link[2].d[0] & 0x01) ) *dsm_out |= 0x400;  //DiEM1  FCS10
    if( (link[0].d[3] & 0x01) && (link[2].d[3] & 0x01) ) *dsm_out |= 0x800;  //DiHAD1 FCS11
    if( (link[0].d[2] & 0x01) && (link[2].d[2] & 0x01) ) *dsm_out |= 0x1000; //DiGAM1 FCS12
    if( (link[0].d[1] & 0x01) && (link[2].d[1] & 0x01) ) *dsm_out |= 0x2000; //DiELE1 FCS13
    if( (link[0].d[6] & 0x04) || (link[2].d[6] & 0x04) ) *dsm_out |= 0x4000; //PresOR FCS14
    *dsm_out |= 0x8000;                                       // always 1 as a marker FCS15

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
