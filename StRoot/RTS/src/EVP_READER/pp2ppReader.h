#ifndef _PP2PP_READER_H_
#define _PP2PP_READER_H_

#include <sys/types.h>

#include <daqFormats.h>	// for the size constants


#define	MAXSEQ		4
#define	MAXCHAIN	4
#define	MAXSVX		6
#define	MAXCHAN		128


struct pp2pp_t {
	int mode ;	// 0 normal, 1 ped
	int channels ;
	int max_channels ;

	void *p_data[3] ;	// points to raw banks

	// camac parts
	struct camac {
		u_int xing ;
		u_int type ;
		u_int seq ;
		u_int token ;
		u_int len ;	// unused for camac
		u_int d[21][12] ;	// 21 slots of (up to) 12 32bit quantities each...
	} cam ;

	struct silicon {
		u_int len ;
		u_int token ;
		u_int seq ;
		u_int xing ;
		u_int type ;
		unsigned char d[MAXSEQ][MAXCHAIN][MAXSVX][MAXCHAN] ;
		unsigned char err[MAXSEQ][MAXCHAIN] ;
	} sec[2] ;


} ;

extern struct pp2pp_t pp2pp ;

extern int pp2ppReader(char *mem) ;

#endif
