#ifndef _RIC_READER_H_
#define _RIC_READER_H_

#include <sys/types.h>


struct ric {
	int mode ;
	int channels ;
	int max_channels ;	// 16*960

	u_short adc[16][960] ;
} ;

extern struct ric ric ;

extern int ricReader(char *mem) ;

#endif
