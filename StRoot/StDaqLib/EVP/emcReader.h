#ifndef _EMC_READER_H_
#define _EMC_READER_H_

#include <sys/types.h>


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

	u_char etow_in ;
	u_short etow_max_ch ;
	u_short etow_ch ;
	u_short etow[4800] ;	// noooo idea...
	u_short *etow_raw ;

	u_char esmd_in ;
	u_short esmd_max_ch ;
	u_short esmd_ch ;
	u_short esmd[4800] ;	// noooo idea...
	u_char  esmd_cap[8] ;

	u_char epre_in ;
	u_short epre_max_ch ;
	u_short epre_ch ;
	u_short epre[4800] ;	// noooo idea...

} ;

extern struct emc emc ;

extern int emcReader(char *mem) ;
extern int getEemcTower(int r) ; // Added by Herbert Ward.

#endif
