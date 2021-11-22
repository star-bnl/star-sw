#ifndef _FST_PED_HH_
#define _FST_PED_HH_


#include <sys/types.h>

#include "daq_fgt.h"

//#define FGT_CH_STAT_SHOULD              0x01    // exists in hardware
//#define FGT_CH_STAT_NO_CONFIG           0x02    // killed in RC or config file
//#define FGT_CH_STAT_NO_RESPONSE         0x04    // killed in ARS_configure, non responding CH
//#define FGT_CH_STAT_BAD                 0x08    // killed in bad_channel
//#define FGT_CH_STAT_PED_UNKNOWN		0x10	// never had a good pedestal calculated

class fstPed {
public:
	fstPed(int rts_id) ;
	~fstPed() { ; } ;

	double nSigmaCut ;

	double nSeedsCut ;
	int nSeedsTbs ;

	double nPedsCut1 ;
	int nPedsTbs1 ;

	float nPedsCut2 ;
	int nPedsTbs2 ;

	int valid ;	// when calced or loaded

	void init(int active_rbs) ;					// mallocs (if nece) and clears ped_store

	void clear() ;
	void clear_from_cache() ;

	void accum(char *evbuff, int bytes, int rdo1) ;

	void calc() ;					// calculates mean/rms into ped_store
	int to_evb(char *buff) ;			// to EVB format from ped_store

	double do_thresh(double n_sigma, int k_seq, int log_bad) ;

	int do_zs(char *src, int in_bytes, char *dst, int rdo1, int id) ;
	int run_stop() ;	// prints errors etc.

	int from_cache(char *fname = 0) ;		// from cached file to ped_store
	int to_cache(char *fname, u_int run, int dont_cache) ;			// to cached file from ped_store
	int bad_from_cache(char *fname = 0) ;


	int sector ;		// 1 or 2

	int evts_for_rms ;
	int evts_for_cmn ;

	int tb_cou_xpect ;	// as set in the conf file "ntimebins"!
	int tb_cou_ped ;	// as in the pedestals/load file!

	u_int total_charge ;	// summed up in the do_zs

	u_char ch_status[FGT_RDO_COU][FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU] ;

	static char cmnGroup[24][128] ;	// apv,ch

	static int load_group(const char *fname=0) ;

//private:
	// allocated per RDO



	struct peds_t {
		float cmn_ped[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FST_TB_COU] ;	// common noise stuff for FST
		double cmn_rms[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FST_TB_COU] ;	// common noise stuff for FST

		float ped[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FST_TB_COU] ;
		double rms[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FST_TB_COU] ;

		u_short cou[FGT_ARM_COU][FGT_APV_COU][FGT_CH_COU][FST_TB_COU] ;
	} peds[FGT_RDO_COU] ; // peds_from_cache[FGT_RDO_COU] ;	// I'm not sure I need this 2nd one at all


	daq_fgt *fgt_rdr[8] ;	// need this for ADC unpacking!

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
