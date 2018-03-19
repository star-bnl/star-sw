#ifndef _ITPC_FCF_H_
#define _ITPC_FCF_H_

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <sys/types.h>
#include <stdlib.h>
#include <time.h>
#include <arpa/inet.h>
#include <sys/time.h>


#include <rtsLog.h>	// for my LOG() call
#include <rtsSystems.h>

// this needs to be always included
#include <DAQ_READER/daqReader.h>
#include <DAQ_READER/daq_dta.h>

#include <trgDataDefs.h>
#include "trgConfNum.h"

// only the detectors we will use need to be included
// for their structure definitions...
#include <DAQ_TPX/daq_tpx.h>
#include <DAQ_TPX/tpxFCF_flags.h>

#include <DAQ_ITPC/daq_itpc.h>
#include <DAQ_ITPC/itpcCore.h>
#include <DAQ_ITPC/itpcPed.h>
#include <DAQ_ITPC/itpcInterpreter.h>
#include <DAQ_ITPC/itpc_rowlen.h>


class itpc_fcf_c : public itpcPed {
public:
	itpc_fcf_c() { 
		want_data = 1 ; 
		my_id = -1 ;

		init(-1) ; 
		run_start() ;	// just in case

		s1_found = 0 ;

		version = 0x20180000 ;
		words_per_cluster = 0 ;	// set in stage3!
	} ;

	~itpc_fcf_c() { ; } ;


	int init(int sector) ;
	void run_start() ;
	void run_stop() ;

	void event_start() ;	// mostly to zap some debugging counters

        int do_ch(int fee_id, int fee_ch, u_int *data, int words) ;

	int version ;

	int do_fcf(void *storage, int bytes) ;


	static int fcf_decode(unsigned int *p_buff, daq_cld *dc, unsigned int version) ;

	int my_id ;

	struct f_stat_t {
		double tm[10] ;
		u_int evt_cou ;

		u_int toobigs ;

		u_int max_s1_found ;
		u_int max_s1_len ;
		u_int max_blob_cou ;
	} f_stat ;

private:

	static const int MAX_ROW = 40 ;		// row or timebin
	static const int MAX_PAD = 120 ;	// pad or pad
	static const int MAX_TB = 512 ;		// tb or row

	static const int MAX_BLOB = 16*1024 ;	// possibly realloced dynamically?
	static const int MAX_PEAKS = 1000 ;	// per blob! This should not be dramatically high

	// used for actual physical ADC channels; big enough for TPX and iTPC!
	static const int MAX_PHYS_ROW	= 45 ;
	static const int MAX_PHYS_PAD	= 182 ;

	int words_per_cluster ;

	int rowlen[MAX_ROW+1] ;

	u_short blob_ix[MAX_BLOB] ;

	struct blob_t {
		int cou ;
		int merges ;

		u_short seq_cou ;
		u_short p1, p2 ;
		u_short t1, t2 ;

		u_short flags ;

		float tot_charge ;
		u_short pixels ;

	} blob[MAX_BLOB] ;

	u_short blob_cou ;



	struct {
		u_short i ;
		u_short j ;
		u_short adc ;
	} peaks[MAX_PEAKS+1] ;


	u_int *out_store ;
	int max_out_bytes ;


	short smooth_dta[64*1024] ;	// for smoothing

	// ACTUAL: physical electronics gains!!!
	struct gain_rp_t {
		float gain ;
		float t0 ;
		u_char flags ;

	} gain_row_pad[MAX_ROW+1][MAX_PAD+1] ;

	// this is where I recopy over data
	struct rp_t {
		u_short s1_data[MAX_TB] ;
		u_short s1_len ;
	} row_pad[MAX_ROW+1][MAX_PAD+1] ;	// [row-like][pad-like]

	int s1_found ;

//	int max_s1_len ;
//	int max_s1_found ;
//	int max_blob_cou ;

	int do_blobs_stage1(int row) ;
	int do_blobs_stage2(int row) ;
	int do_blobs_stage3(int row) ;


	int do_row_check(int row) ;


} ;

#endif
