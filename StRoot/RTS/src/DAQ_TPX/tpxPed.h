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


	int clock_source ;	// 0 TCD, 1 RCC-local, 3 RCC-RHIC, 9-unknown
	int sector ; // if fee is overriden...
	int valid ;	// when calced or loaded

	void init(int active_rbs) ;					// mallocs (if nece) and clears ped_store
	void accum(char *evbuff, int bytes) ;
	void calc() ;					// calculates mean/rms into ped_store

	int to_altro(char *buff, int rb, int timebins) ;		// to ALTRO format from ped_store

	int to_evb(char *buff) ;			// to EVB format from ped_store
	int from_evb(char *buff, int bytes) ;		// decode from EVB to ped_store

	int from_cache(char *fname = 0, u_int r_mask = 0x3F) ;		// from cached file to ped_store
	int to_cache(char *fname = 0, u_int run = 0) ;			// to cached file from ped_store

	int special_setup(int run_type, int sub_type) ;

	void kill_bad(int row, int pad) ;		// kills this specific pad in ped_store


	void smooth() ;					// from ped_store to ped_store
	int summarize(FILE *log=0) ;

	int max_events ;	// max events allowed in the calculation

	int rb_mask ;
private:
	struct peds {
		double ped[512] ;
		double rms[512] ;
		u_short cou[512] ;
	} *ped_store ;

	int smoothed ;	// boolean



	int sizeof_ped ;

	u_int evts[6] ;	// RDOs count from 0 here!
	u_int valid_evts[6] ;


	void accum(tpx_altro_struct *a) ;	// adds values into ped_store

	struct peds *get(int row, int pad) {		// returns pointer to ped_store
		return (ped_store + row*183 + pad) ;
	}
} ;

#endif
