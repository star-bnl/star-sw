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


//#include <rtsLog.h>	// for my LOG() call
//#include <rtsSystems.h>

// this needs to be always included
//#include <DAQ_READER/daqReader.h>
//#include <DAQ_READER/daq_dta.h>

//#include <trgDataDefs.h>
//#include "trgConfNum.h"

// only the detectors we will use need to be included
// for their structure definitions...
//#include <DAQ_TPX/daq_tpx.h>
//#include <DAQ_TPX/tpxFCF_flags.h>

//#include <DAQ_ITPC/daq_itpc.h>
//#include <DAQ_ITPC/itpcCore.h>
#include <DAQ_ITPC/itpcPed.h>
//#include <DAQ_ITPC/itpcInterpreter.h>
//#include <DAQ_ITPC/itpc_rowlen.h>

struct daq_dta ;

class itpc_fcf_c : public itpcPed {
public:
	itpc_fcf_c() ;
	~itpc_fcf_c() ;



	void run_start() ;
	void run_stop() ;
	void event_start() ;	// mostly to zap some debugging counters

        int do_ch(int fee_id, int fee_ch, u_int *data, int words) ;
        int do_ch_sim(int row, int pad, u_short *tb_buff, u_short *track_id) ;

	int do_fcf(void *storage, int bytes) ;


	int init(int sector, const char *fname=0) ;
	int init(daq_dta *gain) ;

	static int get_bad(int sec1, int row1, int pad1) ;
	static float get_gain(int sec1, int row1, int pad1) ;
	static int fcf_decode(unsigned int *p_buff, daq_cld *dc, unsigned int version) ;
	static int fcf_decode(unsigned int *p_buff, daq_sim_cld_x *dc, unsigned int version) ;

	int my_id ;
	int version ;
	int sector_id ;
	int offline ;

	int words_per_cluster ;

	// statistics

	struct f_stat_t {
		double tm[10] ;
		u_int evt_cou ;

		u_int toobigs ;
		
		u_int s1_found ;	// storage during 1 event

		u_int max_s1_found ;
		u_int max_s1_len ;
		u_int max_blob_cou ;
	} f_stat ;

	int det_type ;		// TPC=0, ITPC=1, OTHER=2
	int y_is_timebin ;	// 1=normal cluster finder ("across-rows") (x=pad,y=timebin,slice=row)
				// 0=across timebin ("across-timebins") (x=pad,y=row,slice=timebin)

	int max_x ;		// max pad ever e.g. 182 TPC or 120 ITPC or any other number for OTHER 
	int max_y ;		// max timebin or row ever e.g. 512 
	int max_slice ;		// rows or timebins e.g. 40 ITPC, 45 TPC or 512 for across-timebin

	int pad_to_x(int pad, int y) ;
private:

	static const int MAX_SEC = 24 ;		// sectors; stay as a constant

//	static const int MAX_ROW = 40 ;		// row or timebin; 40,45,512
//	static const int MAX_PAD = 120 ;	// pad or pad; 182, 120
//	static const int MAX_TB = 512 ;		// timebin or row; 512, 40, 45

	static const int MAX_TB_EVER = 512 ;	// for some simple allocations
	static const int MAX_BLOB = 16*1024 ;	// possibly realloced dynamically?
	static const int MAX_PEAKS = 1000 ;	// per blob! This should not be dramatically high
	static const int MAX_BLOB_SIZE = 64*1024 ;

	// used for actual physical ADC channels, mostly for the gain correction; big enough for TPX and iTPC!
	static const int MAX_PHYS_ROW	= 45 ;
	static const int MAX_PHYS_PAD	= 182 ;



//	static int rowlen[45+1] ;	// depends on ITPC vs TPX; but is _constant_ 120 or 182 for timebin-across
					// so I will dimension it for the largest case and that is 45!
	int x_max(int slice, int y) ;
	int x_min(int slice, int y) ;
	int y_max() ;
	int y_min() ;




	// ************ gains and flags
	// ACTUAL: physical electronics gains!!!
	struct gain_rp_t {
		float gain ;
		float t0 ;
		u_char flags ;
	} ;

	// I keep and allocate statically for ALL sectors
	static gain_rp_t *sec_gains[MAX_SEC+1] ;

	gain_rp_t *get_gain(int phys_row, int phys_pad) ;

	int use_gain ;

	// ********* input stage (do_ch)
	// for do_ch and itpc unpacking
	u_short tb_buff[MAX_TB_EVER] ;	// where I stohre the y data -- just max it out

	// this is where we store unpacked data from the detector or user input
	int s1_data_length ;
	struct rp_t {
		u_short s1_len ;
		u_short s1_data[] ;
	} ;

	u_short *row_pad_store ;

	inline struct rp_t *get_row_pad(int row, int pad) ;

	// ********** stage 1
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

	// *********** stage 2 -- various extents


	// *********** stage 3 

	short smooth_dta[MAX_BLOB_SIZE] ;	// for smoothing
	u_short *track_dta ;	// for Offline simulation

	struct {
		u_short i ;
		u_short j ;
		u_short adc ;
	} peaks[MAX_PEAKS+1] ;

	// misc
	u_int blob_id ;		// for debugging

	// final output 
	u_int *out_store ;
	int max_out_bytes ;

	



	int do_blobs_stage1(int row) ;
	int do_blobs_stage2(int row) ;
	int do_blobs_stage3(int row) ;


	int do_row_check(int row) ;


} ;

#endif
