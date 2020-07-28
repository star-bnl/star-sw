#ifndef _FCS_TRG_0_H_
#define _FCS_TRG_0_H_

#include "fcs_trg.h"

enum {kNS=2, kDet=3, kDep=20, kCh=32, kEcalDep=20, kHcalDep=8, kPresDep=4, kLink2=2}; 

// Thresholds
extern u_int EMTHR1;
extern u_int EMTHR2;
extern u_int EMTHR3;
extern u_int HADTHR1;
extern u_int HADTHR2;
extern u_int HADTHR3;
extern u_int JETTHR1;
extern u_int JETTHR2;
extern u_int JETTHR3;
extern float EM_HERATIO_THR;
extern float HAD_HERATIO_THR;
extern float FPSTHR;

// common functions
u_short fcs_trg_run_0(int trgSelect=0, int debug=0);

// Stage 0 is where the raw ADCs get summed, pedestal subtracted, gain corrected.
// This I expect to do within the framework.
// The example below should be pretty finished.
 
int stage_0(short radc, int timebin, geom_t geo, ped_gain_t *pg, u_int *dta_out) ;

// First stage Trigger algorithm: local to any DEP board.
// Inputs are 32 bit ADC values, output is what gets sent on the outgoing link.
// Processing is detector dependent thus one expects to use the geometry.

int stage_1_0(u_int adc[kCh], geom_t geo, link_t *output) ;

// Processing on the North or South DEP/IO flavoured board. 
// Inputs are up to 32 links but I already organized them according to strawman.
// output is 1 link over the external OUT connector.
// Right now we assume we have 20 inputs from ECAL, 6 from HCAL and 4 from PRE.
// We also assume there is no need to know if this is North or South as
// the processing is exactly the same. Right??

int stage_2_0(link_t ecal[kEcalDep], link_t hcal[kHcalDep], link_t pre[kPresDep], geom_t geo, link_t output[kLink2]) ;

// And the last stage where North and South are combined.
// Output is a 12 bit value (max) which goes into STAR Trigger
// either LastDSM, RAT, TCU, etc.

int stage_3_0(link_t link[kNS*kLink2], u_short *dsm_out) ;

#endif
