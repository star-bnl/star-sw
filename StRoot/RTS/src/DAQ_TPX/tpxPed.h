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

	int valid ;		// when calced or loaded

	void init(int sec, int active_rbs) ;					// mallocs (if nece) and clears ped_store
	void clear() ;	// zaps storage

	void accum(char *evbuff, int bytes) ;
	void calc() ;					// calculates mean/rms into ped_store

	int to_altro(char *buff, int rb, int timebins) ;// to ALTRO format from ped_store

	int to_evb(char *buff) ;			// to EVB format from ped_store

	int from_cache(char *fname = 0, u_int r_mask = 0x3F) ;		// from cached file to ped_store
	int to_cache(char *fname = 0, u_int run = 0) ;			// to cached file from ped_store

	int special_setup(int run_type, int sub_type) ;
	int hlt_debug_setup(int param) ;

	int kill_bad(int r0_logical,int row, int pad) ;		// kills this specific pad in ped_store

	void smooth() ;					// from ped_store to ped_store

	int max_events ;	// max events allowed in the calculation


private:

	int sector ;		// logical (1..36)!
	int rb_mask ;	// logical mask

	int smoothed ;	// boolean


	struct peds {
		u_short row ;
		u_short pad ;

		double ped[512] ;
		double rms[512] ;
		u_short cou[512] ;
	} ; // *ped_store ;

	struct peds_rdo_t {
		int r_real ;
		int s_real ;

		struct peds *peds ;
		short ix[46][183] ;
	} ped_rdo_store[6] ;	// indexed by logical r0


	u_int evts[6] ;		// logical r0: RDOs count from 0 here!
	u_int valid_evts[6] ;	// logical r0


	void accum(tpx_altro_struct *a) ;	// adds values into ped_store

	struct peds *get(int r0_logical, int row, int pad) {		// returns pointer to ped_store
		if(rb_mask & (1<<r0_logical)) ;
		else return 0 ;	// not enabled!

		if(ped_rdo_store[r0_logical].peds == 0) {
			LOG(ERR,"What????") ;
			return 0 ;
		}


		int ix = ped_rdo_store[r0_logical].ix[row][pad] ;

		if(ix >= 0) {
			return ped_rdo_store[r0_logical].peds + ix ;
		}
	
#if 0
		for(int i=0;i<1152;i++) {
			if(ped_rdo_store[r0_logical].peds[i].row == row) {
				//LOG(WARN,"Got row %d",row) ;
				if(ped_rdo_store[r0_logical].peds[i].pad == pad) {
					//LOG(WARN,"  got pad %d",pad) ;
					return (ped_rdo_store[r0_logical].peds + i) ;
				}
			}
		}
#endif
		//LOG(ERR,"No row pad %d %d???",row,pad) ;

		return 0 ;
	}


} ;

#endif
