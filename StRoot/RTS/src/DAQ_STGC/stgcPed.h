#ifndef _STGC_PED_HH_
#define _STGC_PED_HH_


#include <sys/types.h>

#include <DAQ_TPX/tpxCore.h>


/*
	Operates on ONE sector only!
*/

class stgcPed {
public:
	stgcPed() ;
	~stgcPed() ;



	int valid ;		// when calced or loaded

	void init(int sec, int active_rbs) ;					// mallocs (if nece) and clears ped_store
	void clear() ;	// zaps storage

	void accum(char *evbuff, int bytes) ;
	void calc() ;					// calculates mean/rms into ped_store

	int to_altro(char *buff, int rb, int timebins) ;// to ALTRO format from ped_store

	int to_evb(char *buff) ;			// to EVB format from ped_store

	int from_cache(char *fname = 0, u_int r_mask = 0x3F) ;		// from cached file to ped_store
	int to_cache(char *fname = 0, u_int run = 0) ;			// to cached file from ped_store



	int kill_bad(int r0_logical,int altro, int ch) ;		// kills this specific pad in ped_store

	void smooth() ;
	
	int max_events ;	// max events allowed in the calculation


private:

	int sector ;
	int rb_mask ;

	int evts[6] ;
	int valid_evts[6] ;
	int altro_found[256] ;

	struct peds {
		double ped[512] ;
		double rms[512] ;
		u_short cou[512] ;
	} peds[256][16] ;	// ALTRO:ch


	void accum(tpx_altro_struct *a) ;

} ;

#endif
