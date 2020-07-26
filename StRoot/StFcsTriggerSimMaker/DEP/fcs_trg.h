#ifndef _FCS_TRG_H_
#define _FCS_TRG_H_

#include <sys/types.h>

// Serial link abstraction type. I want it this way because this is how
// it actually works in the FPGA: 8 strobes of 8 bits
struct link_t {
	u_char d[8] ;	// thus 8x8=64 bits
};

// GLobal geometry type which can be used by the procedures below if
// they need to "know" who they are. 
struct geom_t {
	u_char ns ;	// 0=north,1=south
	u_char det ;	// 0=ecal,1=hcal,2=pre
	u_char dep ;	// 0..22 e.g. ECAL has 23 boards per side, others much less
	u_char ch ;	// 0..31 used only for debugging really
} ;


// Per-channel pedestal/gain-correction tables. Used only in Stage 0
// machinations.
struct ped_gain_t {
	u_short ped ;	// 15 bits max
	u_short gain ;	// 10 bits : 4.6 fixed point
} ;

enum {kMaxNS=2, kMaxDet=3, kMaxDep=20, kMaxCh=32, kMaxEcalDep=20, kMaxHcalDep=8, kMaxPresDep=4, kMaxLink2=2};

// global storage                                                                                                                                      
extern u_int   fcs_trg_sim_adc[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;
extern float   fcs_trg_pt_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
extern float   fcs_trg_gain_correction[kMaxNS][kMaxDet][kMaxDep][kMaxCh];
extern u_short fcs_trg_pedestal[kMaxNS][kMaxDet][kMaxDep][kMaxCh] ;

extern int fcs_trgSelect;
extern int fcs_trgDebug;

u_short fcs_trg_run(int trgSelect, int debug);

#endif
