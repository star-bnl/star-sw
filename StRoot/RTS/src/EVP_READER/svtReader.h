#ifndef _SVT_READER_H_
#define _SVT_READER_H_

#include <sys/types.h>

struct svt_t {
	int channels ;
	int mode ;	// 0 normal, 1 pedestals/RMSs
	int max_channels ;
	int pre, post, pedoffset ;

	// how many valid timebins in this hybrid
	u_char counts[24][3][6][240] ;
	// up to 128 valid timebins (count is in counts)
	// timebin is overloaded for pedestal RMS data!
	u_char timebin[24][3][6][240][128] ;

	// up to 128 valid adcs (same count as above...)
	u_char adc[24][3][6][240][128] ;

	// helpers for the remap
	u_char B[24][3][6] ;	// from RB,MZ,ASIC (all start from 1!) to Barrel
	u_char L[24][3][6] ;	// ... to Ladder
	u_char W[24][3][6] ;	// ... to Wafer
	u_char H[24][3][6] ;	// ... to Hybrid
} ;

extern struct svt_t svt ;
extern int svtReader(char *mem) ;



#endif
