#ifndef _ITPC23_H_
#define _ITPC23_H_

#include "tpc23_base.h"

struct daq_dta ;

class itpc23 : public tpc23_base {
public:
	itpc23() ;
	~itpc23() ;

	u_short fee_mask ;	// expected
	u_short fee_words ;	// 0:real FEE, non-0: simulated words

	u_int *d_start ;	// original FIFO c_addr
	u_int *d_now ;		// where am I now
	u_int *trl ;		// start of trailer aka end-of-event

	int words ;		// original FIFO words
	u_int err ;		// cleared at rdo_start


	//current values
	int bx_count ;
	int fee_pp ;	// padplane
	int lane_ix ;	// 0..4
	int ch_ix ;	// 0..15
	int fee_ix ;	// 0..15 aka port
	int fee_evt_type ;

	int prog_fulls ;
	int fee_errs ;	

	int rdo_scan(char *c_addr, int words) ;
	int from22to23(char *dta, int words) ;
	u_int get_token_s(char *c_addr, int words) ;
	//inline void set_rdo(int s, int r) ;   // never defined I removed (jml)

	int init(daq_dta *gain) ;

	static struct row_pad_t (*rp_gain_itpc)[ROW_MAX+1][PAD_MAX+1] ;     // max for both dets; all sectors

	static void itpc_fee_kill(int s0, int r0, int p0) ;

private:
	u_int *fee_non_trgd(u_int *d) ;
	u_int *fee_scan(u_int *d) ;
	u_int *lane_scan(u_int *d) ;
	u_int *ch_scan(u_int *d) ;

	u_char flags_row_pad(int asic, int channel, int &row, int &pad) ;	
} ;

#endif
