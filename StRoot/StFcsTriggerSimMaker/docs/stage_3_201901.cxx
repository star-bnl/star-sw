#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_201901.h"

// And the last stage where North and South are combined.
// Output is a 12 bit value (max) which goes into STAR Trigger
// either LastDSM, RAT, TCU, etc.

int stage_3_201901(link_t link[kNS*kLink2], u_short *dsm_out)
{
    u_int out = 0;
    if((link[1].d[0] & 0x07)>0) out |= 0x1;  //EM1
    if((link[1].d[0] & 0x38)>0) out |= 0x2;  //EM2
    if((link[1].d[1] & 0x07)>0) out |= 0x4;  //GAM1
    if((link[1].d[1] & 0x38)>0) out |= 0x8;  //GAM2
    if((link[1].d[2] & 0x07)>0) out |= 0x10; //ELE1
    if((link[1].d[2] & 0x38)>0) out |= 0x20; //ELE2
    if((link[1].d[3] & 0x07)>0) out |= 0x40; //HAD1
    if((link[1].d[3] & 0x38)>0) out |= 0x80; //HAD2
    if((link[1].d[4] & 0x01)>0) out |= 0x100; //JP1
    if((link[1].d[4] & 0x02)>0) out |= 0x200; //JP2

    *dsm_out=0;
    if((out & TRG_SELECT_201901)>0) *dsm_out=0x1;

    if(fcs_trgDebug>=1) printf("FCS STG3 Trg=%04x Output=%1x\n",out,*dsm_out);
    return 0 ;
}

