#ifndef _TPX_PED_HH_
#define _TPX_PED_HH_


#include <sys/types.h>

#include "tpxCore.h"


/*
	Operates on ONE sector only!
*/

class tpxPed {
public:
	tpxPed() ;
	~tpxPed() ;

	struct peds {
		double ped[512] ;
		double rms[512] ;
		u_short cou[512] ;
	} *ped_store ;


	void init() ;					// mallocs (if nece) and clears ped_store
	void accum(tpx_altro_struct *a) ;	// adds values into ped_store
	void accum(char *evbuff, int bytes) ;

	void calc() ;					// calculates mean/rms into ped_store

	int to_altro(char *buff, int rb, int timebins) ;		// to ALTRO format from ped_store

	int to_evb(char *buff) ;			// to EVB format from ped_store
	int from_evb(char *buff, int bytes) ;		// decode from EVB to ped_store

	int from_cache(char *fname = 0) ;		// from cached file to ped_store
	int to_cache(char *fname = 0) ;			// to cached file from ped_store

	void kill_bad(int row, int pad) ;		// kills this specific pad in ped_store

	u_int evts[6] ;	// RDOs count from 0 here!
	u_int valid_evts[6] ;

	void smooth() ;					// from ped_store to ped_store
	int summarize(FILE *log=0) ;

	int sector ; // if fee is overriden...

private:
	int smoothed ;	// boolean

	int valid ;	// when calced or loaded

	int sizeof_ped ;

	struct peds *get(int row, int pad) ;		// returns pointer to ped_store

} ;

#endif
