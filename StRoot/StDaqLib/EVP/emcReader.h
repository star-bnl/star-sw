#ifndef _EMC_READER_H_
#define _EMC_READER_H_

#include <sys/types.h>


// ENDCAP constants; from Piotr
#define ETOW_MAXFEE      6
#define ETOW_PRESIZE     4
#define ETOW_DATSIZE   160
#define ESMD_MAXFEE     30
#define ESMD_PRESIZE     4
#define ESMD_DATSIZE   192

struct emc {
	u_char btow_in ;
	u_short btow_max_ch ;
	u_short btow_ch ;
	u_short btow[4800] ;
	u_short *btow_raw ;

	u_char bsmd_in ;
	u_short bsmd_max_ch ;
	u_short bsmd_ch ;
	u_short bsmd[8][4800] ;
	u_char  bsmd_cap[8] ;	// capacitor value...

	u_char bpre_in ;
	u_short bpre_max_ch ;
	u_short bpre_ch ;
	u_short bpre[4800] ;	// noooo idea...

	// ENDCAP TOWERS
	u_char etow_in ;	// in this event?
	u_short etow_max_ch ;	// constant ETOW_MAXFEE * ETOW_DATSIZE
	u_short etow_ch ;	// channels above zero
	u_short etow[ETOW_MAXFEE][ETOW_DATSIZE] ;	// ADC data...
	u_short etow_pre[ETOW_MAXFEE][ETOW_PRESIZE]; // ETOW preamble
	u_short *etow_raw ;	// pointer to the beginning of rawdata; raw data is little endian



	// ENDCAP Showermax & preshower(?)
	u_char esmd_in ;	// in this event?
	u_short esmd_max_ch ;	// 30 * 192
	u_short esmd_ch ;	// channels above 0
	u_short esmd[ESMD_MAXFEE][ESMD_DATSIZE] ;	// ADC data
	u_short esmd_pre[ESMD_MAXFEE][ESMD_PRESIZE]; // ESMD preamble
	u_short *esmd_raw ;	// pointer to the beginning of raw data; raw data is little endian


} ;

extern struct emc emc ;

extern int emcReader(char *mem) ;

#endif
