#ifndef _BSMD_PED_HH_
#define _BSMD_PED_HH_


#include <stdint.h>



/*
	Operates on ONE sector only!
*/

class bsmdPed {
public:
	bsmdPed() ;
	~bsmdPed() ;


	int sector ; // if fee is overriden...
	int valid ;	// when calced or loaded

	void init(int active_rbs) ;					// mallocs (if nece) and clears ped_store
	void accum(char *evbuff, int bytes, int rdo) ;
	void calc() ;					// calculates mean/rms into ped_store
	void do_thresh(double n_sm, double n_pre) ;
	int do_zs(char *src, int in_bytes, char *dst, int rdo1, uint32_t *adc_sum=0) ;
	int to_evb(char *buff) ;			// to EVB format from ped_store

	int from_cache(char *fname = 0) ;		// from cached file to ped_store
	int to_cache(char *fname = 0, uint32_t run = 0) ;			// to cached file from ped_store

	int special_setup(int run_type, int sub_type) ;



private:
	struct peds {
		double ped[128][4800] ;
		double rms[128][4800] ;
		uint16_t thr[128][4800] ;
		uint16_t cou[128] ;

	} *ped_store ;


	int sizeof_ped ;

	uint32_t evts[6] ;	// RDOs count from 0 here!
	uint32_t valid_evts[6] ;
	int rb_mask ;

} ;

#endif
