#ifndef _FGT_PED_HH_
#define _FGT_PED_HH_


#include <sys/types.h>

#include "daq_fgt.h"

class fgtPed {
public:
	fgtPed() ;
	~fgtPed() ;


	int sector ; // if fee is overriden...
	int valid ;	// when calced or loaded

	void init(int active_rbs) ;					// mallocs (if nece) and clears ped_store
	void accum(char *evbuff, int bytes, int rdo) ;
	void calc() ;					// calculates mean/rms into ped_store
	void do_thresh(double n_sigma) ;
	int do_zs(char *src, int in_bytes, char *dst, int rdo1) ;
	int to_evb(char *buff) ;			// to EVB format from ped_store

	int from_cache(char *fname = 0) ;		// from cached file to ped_store
	int to_cache(char *fname = 0, u_int run = 0) ;			// to cached file from ped_store

	int special_setup(int run_type, int sub_type) ;



private:
	// allocated per RDO

	// cap-id equivalents for FGT are the 192 timebins
	struct peds {
		double ped[FGT_ARM_COU][FGT_APV_COU][FGT_TB_COU][FGT_CH_COU] ;
		double rms[FGT_ARM_COU][FGT_APV_COU][FGT_TB_COU][FGT_CH_COU] ;
		u_short thr[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU] ;	//  just 1 thr per channel!
		u_short cou[FGT_ARM_COU][FGT_APV_COU][FGT_TB_COU] ;
	} *ped_store ;


	int sizeof_ped ;

	u_int evts[2] ;	// RDOs count from 0 here!
	u_int valid_evts[2] ;
	int rb_mask ;

} ;

#endif
