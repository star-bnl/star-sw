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

	void init(int active_rbs, int rts) ;					// mallocs (if nece) and clears ped_store

	void accum(char *evbuff, int bytes, int rdo1) ;

	void calc() ;					// calculates mean/rms into ped_store
	int to_evb(char *buff) ;			// to EVB format from ped_store

	double do_thresh(double n_sigma, int k_seq) ;

	int do_zs(char *src, int in_bytes, char *dst, int rdo1) ;
	int run_stop() ;	// prints errors etc.

	int from_cache(char *fname = 0) ;		// from cached file to ped_store
	int to_cache(char *fname = 0, u_int run = 0) ;			// to cached file from ped_store

	int special_setup(int run_type, int sub_type) ;

	int rts_id ;

//	int err_counter[FGT_RDO_COU] ;

	int tb_cou_xpect ;

//private:
	// allocated per RDO

	// Note: just 1 pedestal/threshold per channel.
	// Pedestal calc uses tb 1 (or 2nd tb).
	struct peds {
		float ped[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FGT_TB_COU] ;
		float rms[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FGT_TB_COU] ;
		u_short thr[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU] ;
		u_short cou[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FGT_TB_COU] ;	// no need for TB cou!
		u_short expect_cou[FGT_ARM_COU][FGT_APV_COU] ;
	} *ped_store ;


	daq_fgt *fgt_rdr[FGT_RDO_COU] ;

	int sizeof_ped ;

//	u_int evts[FGT_RDO_COU] ;	// RDOs count from 0 here!
//	u_int valid_evts[FGT_RDO_COU] ;

	int rb_mask ;

	int k_seq ;
	double n_sigma ;

	int tb_cou ;

	struct fgt_stat_t {
		int err ;
		int evts ;
		int arm_mask ;
		int err_apv[FGT_ARM_COU][FGT_APV_COU] ;
		int cou_apv[FGT_ARM_COU][FGT_APV_COU] ;
	} fgt_stat[FGT_RDO_COU] ;	// counts from 0!
} ;

#endif
