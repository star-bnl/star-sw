#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"

//#include "fcs_trg_201901.h"

// And the last stage where North and South are combined.
// Output is a 12 bit value (max) which goes into STAR Trigger
// either LastDSM, RAT, TCU, etc.

// Tonko: it goes first into the RAT and then to the LastDSM

void fcs_trg_base::stage_3_202201(link_t link[], u_short *dsm_out)
{
    *dsm_out = 0;

    if( (link[0].d[0] & 0x01) || (link[2].d[0] & 0x01) ) *dsm_out |= 0x1;    //EM1 
    if( (link[0].d[0] & 0x02) || (link[2].d[0] & 0x02) ) *dsm_out |= 0x2;    //EM2 
    if( (link[0].d[0] & 0x01) && (link[2].d[0] & 0x01) ) *dsm_out |= 0x4;    //EM1 N*S
    if( (link[0].d[1] & 0x01) || (link[2].d[1] & 0x01) ) *dsm_out |= 0x8;    //ELE1
    if( (link[0].d[1] & 0x02) || (link[2].d[1] & 0x02) ) *dsm_out |= 0x10;   //ELE2
    if( (link[0].d[1] & 0x01) && (link[2].d[1] & 0x01) ) *dsm_out |= 0x20;   //ELE1 N*S
    if( (link[0].d[2] & 0x01) || (link[2].d[2] & 0x01) ) *dsm_out |= 0x40;   //GAM1
    if( (link[0].d[2] & 0x02) || (link[2].d[2] & 0x02) ) *dsm_out |= 0x80;   //GAM2
    if( (link[0].d[3] & 0x01) || (link[2].d[3] & 0x01) ) *dsm_out |= 0x100;  //HAD1
    if( (link[0].d[4] & 0x01) || (link[2].d[4] & 0x01) ) *dsm_out |= 0x200;  //JP1
    if( (link[0].d[5] & 0x01) || (link[2].d[5] & 0x01) ) *dsm_out |= 0x400;  //ETOT
    if( (link[0].d[5] & 0x02) || (link[2].d[5] & 0x02) ) *dsm_out |= 0x800;  //HTOT
   
    dsmout = *dsm_out;

    if(fcs_trgDebug>=1) {
	printf("FCS STG3 input 0 = %02x %02x %02x %02x %02x %02x %02x %02x\n",
	       link[0].d[0],link[0].d[1],link[0].d[2],link[0].d[3],
	       link[0].d[4],link[0].d[5],link[0].d[6],link[0].d[7]);
	printf("FCS STG3 input 1 = %02x %02x %02x %02x %02x %02x %02x %02x\n",
	       link[2].d[0],link[2].d[1],link[2].d[2],link[2].d[3],
	       link[2].d[4],link[2].d[5],link[2].d[6],link[2].d[7]);
	printf("FCS STG3 output = %04x EM=%1d%1d%1d ELE=%1d%1d%1d GAM=%1d%1d HAD=%1d JP=%1d TOT=%1d%1d\n",
	       *dsm_out,
	       (*dsm_out)>>0 & 0x1,(*dsm_out)>>1 & 0x1,(*dsm_out)>>2 & 0x1,
	       (*dsm_out)>>3 & 0x1,(*dsm_out)>>4 & 0x1,(*dsm_out)>>5 & 0x1,
	       (*dsm_out)>>6 & 0x1,(*dsm_out)>>7 & 0x1,
	       (*dsm_out)>>8 & 0x1,
	       (*dsm_out)>>9 & 0x1,
	       (*dsm_out)>>10 & 0x1,(*dsm_out)>>11 & 0x1);
    }
}
