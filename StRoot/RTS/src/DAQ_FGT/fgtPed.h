#ifndef _FGT_PED_HH_
#define _FGT_PED_HH_


#include <sys/types.h>

#include "daq_fgt.h"

#define FGT_CH_STAT_SHOULD              0x01    // exists in hardware
#define FGT_CH_STAT_NO_CONFIG           0x02    // killed in RC or config file
#define FGT_CH_STAT_NO_RESPONSE         0x04    // killed in ARS_configure, non responding CH
#define FGT_CH_STAT_BAD                 0x08    // killed in bad_channel
#define FGT_CH_STAT_PED_UNKNOWN		0x10	// never had a good pedestal calculated

class fgtPed {
public:
	fgtPed(int rts_id) ;
	~fgtPed() { ; } ;


	int valid ;	// when calced or loaded

	void init(int active_rbs) ;					// mallocs (if nece) and clears ped_store

	void clear() ;
	void clear_from_cache() ;

	void accum(char *evbuff, int bytes, int rdo1) ;

	void calc() ;					// calculates mean/rms into ped_store
	int to_evb(char *buff) ;			// to EVB format from ped_store

	double do_thresh(double n_sigma, int k_seq, int log_bad) ;

	int do_zs(char *src, int in_bytes, char *dst, int rdo1) ;
	int run_stop() ;	// prints errors etc.

	int from_cache(char *fname = 0) ;		// from cached file to ped_store
	int to_cache(char *fname, u_int run, int dont_cache) ;			// to cached file from ped_store
	int bad_from_cache(char *fname = 0) ;


	int rts_id ;		//IST,FMT,FGT...

	int tb_cou_xpect ;	// as set in the conf file "ntimebins"!
	int tb_cou_ped ;	// as in the pedestals/load file!

	u_int total_charge ;	// summed up in the do_zs

	u_char ch_status[FGT_RDO_COU][FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU] ;

//private:
	// allocated per RDO


	struct peds_t {
		float ped[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FGT_TB_COU] ;
		float rms[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FGT_TB_COU] ;
		u_short thr[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU] ;
		u_short cou[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU] ;	// no need for TB cou!
	} peds[FGT_RDO_COU], peds_from_cache[FGT_RDO_COU] ;


	daq_fgt *fgt_rdr[FGT_RDO_COU] ;	// need this for ADC unpacking!

	int rb_mask ;

	int k_seq ;
	double n_sigma ;
	
	struct fgt_stat_t {
		int err ;
		int evts ;
		int arm_mask ;


		int err_apv[FGT_ARM_COU][FGT_APV_COU] ;
		int cou_apv[FGT_ARM_COU][FGT_APV_COU] ;
	} fgt_stat[FGT_RDO_COU] ;	// counts from 0!
} ;

#endif
