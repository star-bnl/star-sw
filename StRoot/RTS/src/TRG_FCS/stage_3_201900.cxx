#include <stdio.h>
#include <sys/types.h>
#include <string.h>

#include "fcs_trg_base.h"

//#include "fcs_trg_201901.h"

// And the last stage where North and South are combined.
// Output is a 12 bit value (max) which goes into STAR Trigger
// either LastDSM, RAT, TCU, etc.

// Tonko: it goes first into the RAT and then to the LastDSM

void fcs_trg_base::stage_3_201900(link_t link[], u_short *dsm_out)
{
    *dsm_out = 0;

    // Tonko: it is link[2] in FY19 (South, first) and not link[1]
    //        as it was in the original code
    // Tonko: also, why ">0"?? 

    if((link[2].d[0] & 0x07)>0) *dsm_out |= 0x1;
    if((link[2].d[0] & 0x38)>0) *dsm_out |= 0x2;
    if((link[2].d[1] & 0x07)>0) *dsm_out |= 0x4;
    if((link[2].d[1] & 0x38)>0) *dsm_out |= 0x8;
    if((link[2].d[2] & 0x07)>0) *dsm_out |= 0x10;
    if((link[2].d[2] & 0x38)>0) *dsm_out |= 0x20;
    if((link[2].d[3] & 0x07)>0) *dsm_out |= 0x40;
    if((link[2].d[3] & 0x38)>0) *dsm_out |= 0x80;
    if((link[2].d[4] & 0x01)>0) *dsm_out |= 0x100;
    if((link[2].d[4] & 0x02)>0) *dsm_out |= 0x200;

    //Tonko: use 12th bit
    if(link[2].d[7] & 0x80) *dsm_out |= 0x800;


    // Tonko: slighly modified so I can run GEANT emulation.
    // This part is ignored in FY19 VHDL since it didn't exist anyway

    if((link[0].d[0] & 0x07)>0) *dsm_out |= 0x1;
    if((link[0].d[0] & 0x38)>0) *dsm_out |= 0x2;
    if((link[0].d[1] & 0x07)>0) *dsm_out |= 0x4;
    if((link[0].d[1] & 0x38)>0) *dsm_out |= 0x8;
    if((link[0].d[2] & 0x07)>0) *dsm_out |= 0x10;
    if((link[0].d[2] & 0x38)>0) *dsm_out |= 0x20;
    if((link[0].d[3] & 0x07)>0) *dsm_out |= 0x40;
    if((link[0].d[3] & 0x38)>0) *dsm_out |= 0x80;
    if((link[0].d[4] & 0x01)>0) *dsm_out |= 0x100;
    if((link[0].d[4] & 0x02)>0) *dsm_out |= 0x200;

    //Tonko: use 12th bit
    if(link[0].d[7] & 0x80) *dsm_out |= 0x800;



    if(fcs_trgDebug>=1) printf("FCS STG3 output = %04x\n",*dsm_out);

}

