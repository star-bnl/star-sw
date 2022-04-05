#ifndef _EMC_READER_H_
#define _EMC_READER_H_

#include <sys/types.h>
#include "daqFormats.h"

//BARREL constants
#define BTOW_MAXFEE	30
#define BTOW_PRESIZE	4
#define BTOW_DATSIZE	160

// ENDCAP constants; from Piotr
#define ETOW_MAXFEE      6
#define ETOW_PRESIZE     4
#define ETOW_DATSIZE   160
#define ESMD_MAXFEE     48	// used to be 30
#define ESMD_PRESIZE     4
#define ESMD_DATSIZE   192

struct emc_t {
        emc_t();
 void   reset();
 int    check();
 
        int fenceA;
	u_char btow_in ;
	u_short btow_max_ch ;
	u_short btow_ch ;
	u_short btow[4800] ; int fenceB;
	u_short *btow_raw ;
	// added later
	u_short btow_new[BTOW_MAXFEE][BTOW_DATSIZE] ;
	u_short btow_pre[BTOW_MAXFEE][BTOW_PRESIZE] ;

	u_char bsmd_in ;
	u_short bsmd_max_ch ;
	u_short bsmd_ch ;
	u_short bsmd[EMC_FIBER_NUM][4800] ;int fenceC;
	u_char  bsmd_cap[EMC_FIBER_NUM]   ;int fenceD;	// capacitor value...

#if 0
	// the bpre is currently unused - the data is in the last 4ish fibers of BSMD...
	u_char bpre_in ;
	u_short bpre_max_ch ;
	u_short bpre_ch ;
	u_short bpre[4800] ;	// noooo idea...
#endif

	// ENDCAP TOWERS
	u_char etow_in ;	// in this event?
	u_short etow_max_ch ;	// constant ETOW_MAXFEE * ETOW_DATSIZE

	u_short etow_ch ;	// channels above zero
	u_short etow[ETOW_MAXFEE][ETOW_DATSIZE]    ; int fenceE;// ADC data...
	u_short etow_pre[ETOW_MAXFEE][ETOW_PRESIZE]; int fenceF;// ETOW preamble
	u_short *etow_raw ;	// pointer to the beginning of rawdata; raw data is little endian



	// ENDCAP Showermax & preshower(?)
	u_char esmd_in ;	// in this event?
	u_short esmd_max_ch ;	// 48 * 192
	u_short esmd_ch ;	// channels above 0
	u_short esmd_max_fee ;	// ESMD_MAXFEE changed between FY04 and FY05...
	u_short esmd    [ESMD_MAXFEE][ESMD_DATSIZE]; int fenceG;	/// ADC data
	u_short esmd_pre[ESMD_MAXFEE][ESMD_PRESIZE]; int fenceH;	/// ESMD preamble
	u_short *esmd_raw ;	// pointer to the beginning of raw data; raw data is little endian
        int fenceZ;


} ;

namespace OLDEVP {
extern struct emc_t emc ;

extern int emcReader(char *mem) ;
}

#endif
