#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"

// And the last stage where North and South are combined.
// For run21, output are 9 bits
// it goes first into the RAT and then to the LastDSM

void fcs_trg_base::stage_3_202207(link_t link[], u_short *dsm_out)
{
    *dsm_out = 0;

    if( (link[1].d[0] & 0x01) || (link[3].d[0] & 0x01) ) *dsm_out |= 0x1;    //HAD0
    if( (link[1].d[0] & 0x02) || (link[3].d[0] & 0x02) ) *dsm_out |= 0x2;    //HAD1
    if( (link[1].d[0] & 0x04) || (link[3].d[0] & 0x04) ) *dsm_out |= 0x4;    //HAD2

    if( (link[0].d[0] & 0x01) || (link[2].d[0] & 0x01) ) *dsm_out |= 0x8;    //EM0
    if( (link[0].d[0] & 0x02) || (link[2].d[0] & 0x02) ) *dsm_out |= 0x10;   //EM1
    if( (link[0].d[0] & 0x04) || (link[2].d[0] & 0x04) ) *dsm_out |= 0x20;   //EM2

    if( (link[0].d[1] & 0x1F) || (link[2].d[1] & 0x1F) ) *dsm_out |= 0x40;   //JP2

    if( (link[1].d[1] & 0x01) || (link[3].d[1] & 0x01) ) *dsm_out |= 0x80;   //JPA1
    if( (link[1].d[1] & 0x06) || (link[3].d[1] & 0x06) ) *dsm_out |= 0x100;  //JPBC1
    if( (link[1].d[1] & 0x18) || (link[3].d[1] & 0x18) ) *dsm_out |= 0x200;  //JPDE1

    if( (link[0].d[2] & 0x01) || (link[2].d[2] & 0x01) ) *dsm_out |= 0x400;  //JPA0
    if( (link[0].d[2] & 0x06) || (link[2].d[2] & 0x06) ) *dsm_out |= 0x800;  //JPBC0
    if( (link[0].d[2] & 0x18) || (link[2].d[2] & 0x18) ) *dsm_out |= 0x1000; //JPDE0

    if( (link[1].d[2] & 0x02) && (link[3].d[2] & 0x1C) ) *dsm_out |= 0x2000; //DiJP
    if( (link[1].d[2] & 0x04) && (link[3].d[2] & 0x1A) ) *dsm_out |= 0x2000; //DiJP
    if( (link[1].d[2] & 0x18) && (link[3].d[2] & 0x1E) ) *dsm_out |= 0x2000; //DiJP

    if( (link[1].d[2] & 0x02) && (link[2].d[2] & 0x1C) ) *dsm_out |= 0x4000; //DiJPAsy
    if( (link[1].d[2] & 0x04) && (link[2].d[2] & 0x1A) ) *dsm_out |= 0x4000; //DiJPAsy
    if( (link[1].d[2] & 0x18) && (link[2].d[2] & 0x1E) ) *dsm_out |= 0x4000; //DiJPAsy
    if( (link[0].d[2] & 0x02) && (link[3].d[2] & 0x1C) ) *dsm_out |= 0x4000; //DiJPAsy
    if( (link[0].d[2] & 0x04) && (link[3].d[2] & 0x1A) ) *dsm_out |= 0x4000; //DiJPAsy
    if( (link[0].d[2] & 0x18) && (link[3].d[2] & 0x1E) ) *dsm_out |= 0x4000; //DiJPAsy

    if( (link[0].d[0] & 0x40) && (link[2].d[0] & 0x20) ) *dsm_out |= 0x8000; //DiELEA
    if( (link[0].d[0] & 0x20) && (link[2].d[0] & 0x40) ) *dsm_out |= 0x8000; //DiELEA

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
