#ifndef _FPD_READER_H_
#define _FPD_READER_H_

#include <sys/types.h>

struct bbc {
	u_short pulse[32] ;
	u_short time[32] ;
	u_short proof[2] ;
	u_short spare[6] ;
	u_short ped[32] ;
	u_short rms[32] ;
	u_short peaks[64] ;
	u_int scl[32] ;
} ;

struct fpd {
	int mode ;
	int channels ;
	int max_channels ;

	u_short adc[256] ;
	u_short tdc[8] ;
	u_short reg[3] ;
	u_short ped[256] ;
	u_short rms[256] ;

	struct bbc bbc ;
		
} ;	

extern struct fpd fpd ;

extern int fpdReader(char *mem) ;

#endif
