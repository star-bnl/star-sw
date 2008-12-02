#ifndef _TOF_READER_H_
#define _TOF_READER_H_

#include <sys/types.h>


struct tof_t {
	int mode ;
	int channels ;
	int max_channels ;

	u_short adc[180] ;	// was 48 in FY02
	u_short tdc[184];	// was 48 in FY02
	float   a2d[32];
	u_int   sca[12];

	// new in FY05
	u_int ddl[4][10000] ;	// content of up to 4 fibers
	u_int ddl_words[4] ;	// the count of words (32bit) for above
} ;

extern struct tof_t tof ;

extern int tofReader(char *mem) ;

#endif
