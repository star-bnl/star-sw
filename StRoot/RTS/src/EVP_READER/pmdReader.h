#ifndef _PMD_READER_H_
#define _PMD_READER_H_

#include <sys/types.h>

#include <daqFormats.h>	// for the size constants

struct pmd {
	int mode ;	// 0 normal, 1 ped
	int channels ;
	int max_channels ;	// 2*10*2*2016

	u_int status[2] ;

	// 2 sectors, 10 CRAMS, 2 CRAM channels, 2016 values max
	u_short adc[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;

	u_short ped[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	u_short rms[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;
	u_short thr[2][PMD_CRAMS_MAX][2][PMD_CRAMS_CH_MAX] ;

} ;

extern struct pmd pmd ;

extern int pmdReader(char *mem) ;

#endif
