#ifndef _ITPC_FCF_H_
#define _ITPC_FCF_H_

#include <stdio.h>
#include <unistd.h>
#include <getopt.h>
#include <stdint.h>
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

        int do_ch(int fee_id, int fee_ch, uint32_t *data, int words) ;
        int do_ch_sim(int row, int pad, uint16_t *tb_buff, uint16_t *track_id) ;

	int do_fcf(void *storage, int bytes) ;


	int init(int sector, const char *fname=0) ;
	int init(daq_dta *gain) ;

	static int get_bad(int sec1, int row1, int pad1) ;
	static float get_gain(int sec1, int row1, int pad1) ;
	static void set_gain(int sec1, int row1, int pad1, float gain) ;

	static uint8_t get_flags(int sec1, int row1, int pad1) ;
	static void set_flags(int sec1, int row1, int pad1, uint8_t flags) ;

	void zap_fee(int sec1, int rdo1, int port1) ;
	
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
		uint32_t evt_cou ;

		uint32_t toobigs ;
		
		uint32_t s1_found ;	// storage during 1 event

		uint32_t max_s1_found ;
		uint32_t max_s1_len ;
		uint32_t max_blob_cou ;
	} f_stat ;

	int det_type ;		// TPC=0, ITPC=1, OTHER=2
	int y_is_timebin ;	// 1=normal cluster finder ("across-rows") (x=pad,y=timebin,slice=row)
				// 0=across timebin ("across-timebins") (x=pad,y=row,slice=timebin)

	int max_x ;		// max pad ever e.g. 182 TPC or 120 ITPC or any other number for OTHER 
	int max_y ;		// max timebin or row ever e.g. 512 
	int max_slice ;		// rows or timebins e.g. 40 ITPC, 45 TPC or 512 for across-timebin

	int pad_to_x(int pad, int y) ;

	int x_max(int slice, int y) ;
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

	int x_min(int slice, int y) ;
	int y_max() ;
	int y_min() ;




	// ************ gains and flags
	// ACTUAL: physical electronics gains!!!
	struct gain_rp_t {
		float gain ;
		float t0 ;
		uint8_t flags ;
	} ;

	// I keep and allocate statically for ALL sectors
	static gain_rp_t *sec_gains[MAX_SEC+1] ;

	gain_rp_t *get_gain(int phys_row, int phys_pad) ;

	int use_gain ;

	// ********* input stage (do_ch)
	// for do_ch and itpc unpacking
	uint16_t tb_buff[MAX_TB_EVER] ;	// where I stohre the y data -- just max it out

	// this is where we store unpacked data from the detector or user input
	int s1_data_length ;
	struct rp_t {
		uint16_t s1_len ;
		uint16_t s1_data[] ;
	} ;

	uint16_t *row_pad_store ;

	inline struct rp_t *get_row_pad(int row, int pad) ;

	// ********** stage 1
	uint16_t blob_ix[MAX_BLOB] ;

	struct blob_t {
		int cou ;
		int merges ;

		uint16_t seq_cou ;
		uint16_t p1, p2 ;
		uint16_t t1, t2 ;

		uint16_t flags ;

		float tot_charge ;
		uint16_t pixels ;

	} blob[MAX_BLOB] ;

	uint16_t blob_cou ;	

	// *********** stage 2 -- various extents


	// *********** stage 3 

	short smooth_dta[MAX_BLOB_SIZE] ;	// for smoothing
	uint16_t *track_dta ;	// for Offline simulation

	struct {
		uint16_t i ;
		uint16_t j ;
		uint16_t adc ;
	} peaks[MAX_PEAKS+1] ;

	// misc
	uint32_t blob_id ;		// for debugging

	// final output 
	uint32_t *out_store ;
	int max_out_bytes ;

	



	int do_blobs_stage1(int row) ;
	int do_blobs_stage2(int row) ;
	int do_blobs_stage3(int row) ;


	int do_row_check(int row) ;


} ;

#endif
